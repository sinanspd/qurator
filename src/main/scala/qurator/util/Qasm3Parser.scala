package qurator.util

import scala.collection.mutable
import scala.util.matching.Regex
import qurator.domain.circuit._
import scala.util.control.NonFatal

object Qasm3Parser {

  sealed trait ParseMode
  object ParseMode {
    case object Strict extends ParseMode
    case object Lenient extends ParseMode
  }

  final case class ParseWarning(
    statement: String,
    message: String
  )

  final case class ParseConfig(
    mode: ParseMode = ParseMode.Strict,
    keepUnknownFlatGates: Boolean = false
  )

  object ParseConfig {
    val strict: ParseConfig =
      ParseConfig(
        mode = ParseMode.Strict,
        keepUnknownFlatGates = false
      )

    val lenientSkipUnsupported: ParseConfig =
      ParseConfig(
        mode = ParseMode.Lenient,
        keepUnknownFlatGates = false
      )

    val lenientKeepNamedUnknown: ParseConfig =
      ParseConfig(
        mode = ParseMode.Lenient,
        keepUnknownFlatGates = true
      )
  }

  final case class ParseReport(
    circuit: Circuit,
    warnings: Vector[ParseWarning]
  )

  sealed trait ParseError extends RuntimeException
  final case class QasmParseError(msg: String) extends RuntimeException(msg) with ParseError

  private sealed trait QRef
  private final case class One(i: Int) extends QRef
  private final case class Many(indices: Vector[Int]) extends QRef

  private final case class Call(
    name: String,
    params: List[String],
    qargs: List[QRef]
  )

  private sealed trait Modifier
  private case object Inv extends Modifier
  private final case class Ctrl(n: Int) extends Modifier

  private val Ident: Regex = "^[A-Za-z_][A-Za-z0-9_]*$".r
  private val IndexedIdent: Regex = "^([A-Za-z_][A-Za-z0-9_]*)\\[(\\d+)\\]$".r
  private val PhysicalQubit: Regex = "^\\$(\\d+)$".r
  private val QubitDecl: Regex = "^qubit(?:\\s*\\[\\s*(\\d+)\\s*\\])?\\s+([A-Za-z_][A-Za-z0-9_]*)$".r
  private val QregDecl: Regex = "^qreg\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\[\\s*(\\d+)\\s*\\]$".r
  private val CregDecl: Regex = "^creg\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\[\\s*(\\d+)\\s*\\]$".r

  def parse(qasm: String, name: String = ""): Circuit =
    parseWithReport(qasm, name, ParseConfig.strict).circuit

  def parseLenient(qasm: String, name: String = ""): ParseReport =
    parseWithReport(qasm, name, ParseConfig.lenientSkipUnsupported)

  def parseWithReport(
    qasm: String,
    name: String = "",
    config: ParseConfig = ParseConfig.strict
  ): ParseReport = {
    val source = stripComments(qasm)

    val qregs = mutable.LinkedHashMap.empty[String, Vector[Int]]
    val warnings = mutable.ListBuffer.empty[ParseWarning]
    val gates = mutable.ListBuffer.empty[Gate]

    var nextQubit = 0
    var maxPhysicalSeen = -1

    def declareQubits(regName: String, size: Int): Unit = {
      if (qregs.contains(regName)) {
        throw QasmParseError(s"Qubit register '$regName' declared twice.")
      }
      val indices = Vector.tabulate(size)(i => nextQubit + i)
      qregs.put(regName, indices)
      nextQubit += size
    }

    def warn(stmt: String, msg: String): Unit =
      warnings += ParseWarning(stmt, msg)

    def handleStatement(stmt: String): Unit = stmt match {
      case s if s.isEmpty =>
        ()

      case s if isIgnorableStatement(s) =>
        ()

      case s if isUnsupportedStructuredStatement(s) =>
        config.mode match {
          case ParseMode.Strict =>
            throw QasmParseError(
              s"Unsupported structured OpenQASM statement: $s"
            )
          case ParseMode.Lenient =>
            warn(s, "Skipped unsupported structured OpenQASM statement")
        }

      case QubitDecl(null, regName) =>
        declareQubits(regName, 1)

      case QubitDecl(sizeStr, regName) =>
        declareQubits(regName, sizeStr.toInt)

      case QregDecl(regName, sizeStr) =>
        declareQubits(regName, sizeStr.toInt)

      case CregDecl(_, _) =>
        ()

      case s if isClassicalDeclaration(s) =>
        ()

      case s if s.startsWith("reset ") =>
        val qargText = s.stripPrefix("reset").trim
        val qrefs = splitTopLevel(qargText, ',').map(tok => resolveQArg(tok.trim, qregs))
        maxPhysicalSeen = math.max(maxPhysicalSeen, maxIndex(qrefs))
        gates ++= broadcast(qrefs).map {
          case List(q) => Reset(q)
          case qs =>
            throw QasmParseError(
              s"reset expects 1 quantum operand, got ${qs.length}: $s"
            )
        }

      case s if isMeasurementStmt(s) =>
        val qargText = extractMeasuredQArg(s)
        val qrefs = splitTopLevel(qargText, ',').map(tok => resolveQArg(tok.trim, qregs))
        maxPhysicalSeen = math.max(maxPhysicalSeen, maxIndex(qrefs))
        gates ++= broadcast(qrefs).map {
          case List(q) => Measure(q)
          case qs =>
            throw QasmParseError(
              s"measure expects 1 quantum operand after broadcasting, got ${qs.length}: $s"
            )
        }

      case s if s.startsWith("barrier ") || s == "barrier" || s.startsWith("nop") =>
        ()

      case s =>
        val (mods, rest) = parseModifiers(s)
        val call = parseCall(rest, qregs)
        maxPhysicalSeen = math.max(maxPhysicalSeen, maxIndex(call.qargs))
        gates ++= lowerCall(call, mods, config)
    }

    val statements = splitTopLevel(source, ';')

    statements.foreach { rawStmt =>
      val stmt = rawStmt.trim
      if (stmt.nonEmpty) {
        try {
          handleStatement(stmt)
        } catch {
          case NonFatal(e) =>
            config.mode match {
              case ParseMode.Strict =>
                throw e
              case ParseMode.Lenient =>
                warn(stmt, e.getMessage)
            }
        }
      }
    }

    val totalQubits = math.max(nextQubit, maxPhysicalSeen + 1)

    ParseReport(
      circuit = Circuit(gates.toList, totalQubits, name),
      warnings = warnings.toVector
    )
  }

  private def isUnsupportedStructuredStatement(s: String): Boolean = {
    val t = s.trim
    t.startsWith("gate ")   ||
    t.startsWith("def ")    ||
    t.startsWith("defcal ") ||
    t.startsWith("cal ")    ||
    t.startsWith("for ")    ||
    t.startsWith("while ")  ||
    t.startsWith("if ")     ||
    t.startsWith("else ")   ||
    t.startsWith("switch ") ||
    t.startsWith("box ")    ||
    t.startsWith("delay ")  ||
    t.contains("{")         ||
    t.contains("}")
  }

  private def isIgnorableStatement(s: String): Boolean = {
    val t = s.trim
    t.startsWith("OPENQASM ") ||
    t.startsWith("include ")  ||
    t.startsWith("pragma ")   ||
    t.startsWith("#pragma ")  ||
    t.startsWith("@")
  }

  private def isClassicalDeclaration(s: String): Boolean = {
    val t = s.trim
    Seq(
      "bit", "bool", "int", "uint", "float", "angle", "complex",
      "const", "input", "output", "array", "let", "stretch", "duration"
    ).exists(kw => t == kw || t.startsWith(kw + " ") || t.startsWith(kw + "["))
  }

  private def isMeasurementStmt(s: String): Boolean = {
    val t = s.trim
    t.startsWith("measure ") || {
      val eq = topLevelEqualsIndex(t)
      eq >= 0 && t.substring(eq + 1).trim.startsWith("measure ")
    }
  }

  private def extractMeasuredQArg(s: String): String = {
    val t = s.trim
    if (t.startsWith("measure ")) {
      val rhs = t.stripPrefix("measure").trim
      splitOnTopLevelArrow(rhs)._1.trim
    } else {
      val eq = topLevelEqualsIndex(t)
      if (eq < 0) throw QasmParseError(s"Invalid measurement statement: $s")
      val rhs = t.substring(eq + 1).trim
      if (!rhs.startsWith("measure ")) throw QasmParseError(s"Invalid measurement statement: $s")
      rhs.stripPrefix("measure").trim
    }
  }

  private def lowerCall(
    call: Call,
    mods: List[Modifier],
    config: ParseConfig
  ): List[Gate] = {
    val inv = mods.count(_ == Inv) % 2 == 1
    val controls = mods.collect { case Ctrl(n) => n }.sum

    val baseCall = if (inv) invertCall(call) else call

    controls match {
      case 0 => lowerSimple(baseCall, config)
      case 1 => lowerControlled(baseCall, 1, config)
      case 2 => lowerControlled(baseCall, 2, config)
      case n =>
        throw QasmParseError(
          s"Only up to 2 controls are lowered into this Gate ADT; got ctrl($n) in ${call.name}."
        )
    }
  }

  private def lowerSimple(
    call: Call,
    config: ParseConfig
  ): List[Gate] = {
    val tuples = broadcast(call.qargs)

    def oneParam(name: String): String = expectParamCount(name, call.params, 1).head
    def twoParams(name: String): (String, String) = {
      val ps = expectParamCount(name, call.params, 2)
      (ps(0), ps(1))
    }
    def threeParams(name: String): (String, String, String) = {
      val ps = expectParamCount(name, call.params, 3)
      (ps(0), ps(1), ps(2))
    }

    call.name match {
      case "x" =>
        tuples.map(expectArity1("x", _)).map(q => X(q))
      case "y" =>
        tuples.map(expectArity1("y", _)).map(q => Y(q))
      case "z" =>
        tuples.map(expectArity1("z", _)).map(q => Z(q))
      case "h" =>
        tuples.map(expectArity1("h", _)).map(q => H(q))
      case "s" =>
        tuples.map(expectArity1("s", _)).map(q => S(q))
      case "sdg" =>
        tuples.map(expectArity1("sdg", _)).map(q => SDG(q))
      case "t" =>
        tuples.map(expectArity1("t", _)).map(q => T(q))
      case "tdg" =>
        tuples.map(expectArity1("tdg", _)).map(q => TDG(q))
      case "sx" =>
        tuples.map(expectArity1("sx", _)).map(q => SX(q))
      case "sxdg" =>
        tuples.map(expectArity1("sxdg", _)).map(q => SXDG(q))
      case "id" =>
        tuples.map(expectArity1("id", _)).map(q => Id(q))

      case "p" | "phase" =>
        val theta = oneParam(call.name)
        tuples.map(expectArity1(call.name, _)).map(q => Phase(theta, q))
      case "rx" =>
        val theta = oneParam("rx")
        tuples.map(expectArity1("rx", _)).map(q => RX(theta, q))
      case "ry" =>
        val theta = oneParam("ry")
        tuples.map(expectArity1("ry", _)).map(q => RY(theta, q))
      case "rz" =>
        val theta = oneParam("rz")
        tuples.map(expectArity1("rz", _)).map(q => RZ(theta, q))
      case "U" | "u" =>
        val (theta, phi, lambda) = threeParams(call.name)
        tuples.map(expectArity1(call.name, _)).map(q => U(theta, phi, lambda, q))
      case "u1" =>
        val lambda = oneParam("u1")
        tuples.map(expectArity1("u1", _)).map(q => Phase(lambda, q))
      case "u2" =>
        val (phi, lambda) = twoParams("u2")
        tuples.map(expectArity1("u2", _)).map(q => U2(phi, lambda, q))
      case "u3" =>
        val (theta, phi, lambda) = threeParams("u3")
        tuples.map(expectArity1("u3", _)).map(q => U3(theta, phi, lambda, q))

      case "cx" | "CX" =>
        tuples.map(expectArity2("cx", _)).map { case (c, t) => CX(c, t) }
      case "cy" =>
        tuples.map(expectArity2("cy", _)).map { case (c, t) => CY(c, t) }
      case "cz" =>
        tuples.map(expectArity2("cz", _)).map { case (c, t) => CZ(c, t) }
      case "ch" =>
        tuples.map(expectArity2("ch", _)).map { case (c, t) => CH(c, t) }
      case "swap" =>
        tuples.map(expectArity2("swap", _)).map { case (a, b) => Swap(a, b) }

      case "cp" | "cphase" =>
        val theta = oneParam(call.name)
        tuples.map(expectArity2(call.name, _)).map { case (c, t) => CP(c, theta, t) }
      case "crx" =>
        val theta = oneParam("crx")
        tuples.map(expectArity2("crx", _)).map { case (c, t) => CRX(c, theta, t) }
      case "cry" =>
        val theta = oneParam("cry")
        tuples.map(expectArity2("cry", _)).map { case (c, t) => CRY(c, theta, t) }
      case "crz" =>
        val theta = oneParam("crz")
        tuples.map(expectArity2("crz", _)).map { case (c, t) => CRZ(c, theta, t) }
      case "rzz" | "RZZ" =>
        val theta = oneParam(call.name)
        tuples.map(expectArity2(call.name, _)).map { case (a, b) =>
          NamedGate("rzz", Vector(theta), Vector(a, b))
        }

      case "ccx" | "CCX" =>
        tuples.map(expectArity3("ccx", _)).map { case (c1, c2, t) => CCX(c1, c2, t) }

      case "gphase" =>
        val gamma = oneParam("gphase")
        tuples.map {
          case Nil => GPhase(gamma)
          case qs =>
            throw QasmParseError(
              s"gphase takes no qubit operands, got ${qs.length}"
            )
        }

      case other if config.keepUnknownFlatGates =>
        tuples.map(qs => NamedGate(other, call.params.toVector, qs.toVector))

      case other =>
        throw QasmParseError(s"Unsupported flat gate '$other'")
    }
  }

  private def lowerControlled(
    call: Call,
    controls: Int,
    config: ParseConfig
  ): List[Gate] = {
    val tuples = broadcast(call.qargs)

    def oneParam(name: String): String = expectParamCount(name, call.params, 1).head
    def threeParams(name: String): (String, String, String) = {
      val ps = expectParamCount(name, call.params, 3)
      (ps(0), ps(1), ps(2))
    }

    (controls, call.name) match {
      case (1, "x") =>
        tuples.map(expectArity2("ctrl @ x", _)).map { case (c, t) => CX(c, t) }
      case (2, "x") =>
        tuples.map(expectArity3("ctrl(2) @ x", _)).map { case (c1, c2, t) => CCX(c1, c2, t) }

      case (1, "y") =>
        tuples.map(expectArity2("ctrl @ y", _)).map { case (c, t) => CY(c, t) }
      case (1, "z") =>
        tuples.map(expectArity2("ctrl @ z", _)).map { case (c, t) => CZ(c, t) }
      case (1, "h") =>
        tuples.map(expectArity2("ctrl @ h", _)).map { case (c, t) => CH(c, t) }

      case (1, "p" | "phase" | "u1") =>
        val theta = oneParam(call.name)
        tuples.map(expectArity2(s"ctrl @ ${call.name}", _)).map { case (c, t) => CP(c, theta, t) }

      case (1, "rx") =>
        val theta = oneParam("rx")
        tuples.map(expectArity2("ctrl @ rx", _)).map { case (c, t) => CRX(c, theta, t) }
      case (1, "ry") =>
        val theta = oneParam("ry")
        tuples.map(expectArity2("ctrl @ ry", _)).map { case (c, t) => CRY(c, theta, t) }
      case (1, "rz") =>
        val theta = oneParam("rz")
        tuples.map(expectArity2("ctrl @ rz", _)).map { case (c, t) => CRZ(c, theta, t) }

      case (1, "U" | "u") =>
        val (theta, phi, lambda) = threeParams(call.name)
        tuples.map(expectArity2("ctrl @ U", _)).map { case (c, t) => CU(c, theta, phi, lambda, t) }

      case (1, "gphase") =>
        val gamma = oneParam("gphase")
        tuples.map(expectArity1("ctrl @ gphase", _)).map(q => U("0", "0", gamma, q))

      case _ =>
        throw QasmParseError(
          s"Unsupported controlled lowering: ${renderModifiers(controls)}${call.name}"
        )
    }
  }
  private def invertCall(call: Call): Call = {
    call.name match {
      case "x" | "y" | "z" | "h" | "cx" | "CX" | "cy" | "cz" | "ch" | "swap" | "ccx" | "CCX" | "id" =>
        call

      case "s"    => call.copy(name = "sdg")
      case "sdg"  => call.copy(name = "s")
      case "t"    => call.copy(name = "tdg")
      case "tdg"  => call.copy(name = "t")
      case "sx"   => call.copy(name = "sxdg")
      case "sxdg" => call.copy(name = "sx")

      case "p" | "phase" | "u1" | "rx" | "ry" | "rz" | "cp" | "cphase" | "crx" | "cry" | "crz" | "rzz" | "RZZ" | "gphase" =>
        call.copy(params = call.params.map(negExpr))

      case "U" | "u" =>
        expectParamCount(call.name, call.params, 3)
        call.copy(params = List(
          negExpr(call.params(0)),
          negExpr(call.params(2)),
          negExpr(call.params(1))
        ))

      case other =>
        throw QasmParseError(s"Unsupported inverse lowering for '$other'.")
    }
  }

  private def parseModifiers(stmt: String): (List[Modifier], String) = {
    val mods = mutable.ListBuffer.empty[Modifier]
    var rest = stmt.trim
    var continue = true

    while (continue) {
      val t = rest.trim
      if (t.startsWith("inv")) {
        val next = stripPrefixModifier(t, "inv")
        mods += Inv
        rest = next
      } else if (t.startsWith("ctrl")) {
        val (n, next) = stripCtrlModifier(t)
        mods += Ctrl(n)
        rest = next
      } else if (t.startsWith("negctrl")) {
        throw QasmParseError("negctrl is not lowered by this parser.")
      } else if (t.startsWith("pow")) {
        throw QasmParseError("pow(k) modifiers are not lowered by this parser.")
      } else {
        continue = false
      }
    }

    (mods.toList, rest.trim)
  }

  private def stripPrefixModifier(s: String, keyword: String): String = {
    val t = s.trim
    if (!t.startsWith(keyword)) throw QasmParseError(s"Expected modifier '$keyword' in '$s'")
    val after = t.substring(keyword.length).trim
    if (!after.startsWith("@")) throw QasmParseError(s"Expected '@' after modifier '$keyword' in '$s'")
    after.substring(1).trim
  }

  private def stripCtrlModifier(s: String): (Int, String) = {
    val t = s.trim
    if (!t.startsWith("ctrl")) throw QasmParseError(s"Expected ctrl modifier in '$s'")
    val afterCtrl = t.substring(4).trim
    val (n, afterN) =
      if (afterCtrl.startsWith("(")) {
        val (inside, nextIndex) = extractBalanced(afterCtrl, 0, '(', ')')
        val n0 = inside.trim.toInt
        (n0, afterCtrl.substring(nextIndex).trim)
      } else {
        (1, afterCtrl)
      }

    if (!afterN.startsWith("@")) throw QasmParseError(s"Expected '@' after ctrl modifier in '$s'")
    (n, afterN.substring(1).trim)
  }

  private def parseCall(stmt: String, qregs: mutable.LinkedHashMap[String, Vector[Int]]): Call = {
    val s = stmt.trim
    if (s.isEmpty) throw QasmParseError("Empty gate call.")

    val nameEnd = s.indexWhere(ch => ch.isWhitespace || ch == '(')
    val (name, rest0) =
      if (nameEnd < 0) (s, "")
      else (s.substring(0, nameEnd), s.substring(nameEnd))

    val rest1 = rest0.trim
    val (params, rest2) =
      if (rest1.startsWith("(")) {
        val (inside, nextIdx) = extractBalanced(rest1, 0, '(', ')')
        (splitTopLevel(inside, ','), rest1.substring(nextIdx).trim)
      } else {
        (Nil, rest1)
      }

    val qargs =
      if (rest2.isEmpty) Nil
      else splitTopLevel(rest2, ',').map(tok => resolveQArg(tok.trim, qregs))

    Call(name, params.map(_.trim).filter(_.nonEmpty), qargs)
  }

  private def resolveQArg(token: String, qregs: mutable.LinkedHashMap[String, Vector[Int]]): QRef = token match {
    case PhysicalQubit(n) =>
      One(n.toInt)

    case IndexedIdent(name, idx) =>
      val reg = qregs.getOrElse(name, throw QasmParseError(s"Unknown qubit register '$name'."))
      val i = idx.toInt
      if (i < 0 || i >= reg.length) {
        throw QasmParseError(s"Index $i out of bounds for register '$name' of size ${reg.length}.")
      }
      One(reg(i))

    case t if Ident.pattern.matcher(t).matches() =>
      val reg = qregs.getOrElse(t, throw QasmParseError(s"Unknown qubit register '$t'."))
      Many(reg)

    case other =>
      throw QasmParseError(
        s"Unsupported quantum operand '$other'. This parser supports identifiers, identifier[index], and physical qubits"
      )
  }

  private def broadcast(qargs: List[QRef]): List[List[Int]] = {
    val sizes = qargs.collect { case Many(v) => v.size }.distinct
    sizes match {
      case Nil =>
        List(qargs.map {
          case One(i)  => i
          case Many(v) =>
            throw QasmParseError(s"Unexpected register broadcast state: $v")
        })
      case n :: Nil =>
        (0 until n).toList.map { i =>
          qargs.map {
            case One(idx) => idx
            case Many(v)  => v(i)
          }
        }
      case many =>
        throw QasmParseError(s"Register broadcast requires all register operands to have the same size, got ${many.mkString(", ")}.")
    }
  }

  private def stripComments(input: String): String = {
    val noBlock = input.replaceAll("(?s)/\\*.*?\\*/", " ")
    noBlock.replaceAll("(?m)//.*$", " ")
  }

  private def splitTopLevel(input: String, sep: Char): List[String] = {
    val out = mutable.ListBuffer.empty[String]
    val sb = new StringBuilder
    var paren = 0
    var bracket = 0
    var brace = 0
    var inString = false
    var escaped = false

    input.foreach { ch =>
      if (inString) {
        sb.append(ch)
        if (escaped) escaped = false
        else if (ch == '\\') escaped = true
        else if (ch == '"') inString = false
      } else {
        ch match {
          case '"' =>
            inString = true
            sb.append(ch)
          case '(' =>
            paren += 1; sb.append(ch)
          case ')' =>
            paren -= 1; sb.append(ch)
          case '[' =>
            bracket += 1; sb.append(ch)
          case ']' =>
            bracket -= 1; sb.append(ch)
          case '{' =>
            brace += 1; sb.append(ch)
          case '}' =>
            brace -= 1; sb.append(ch)
          case c if c == sep && paren == 0 && bracket == 0 && brace == 0 =>
            val piece = sb.toString.trim
            if (piece.nonEmpty) out += piece
            sb.clear()
          case _ =>
            sb.append(ch)
        }
      }
    }

    val last = sb.toString.trim
    if (last.nonEmpty) out += last
    out.toList
  }

  private def extractBalanced(s: String, startIndex: Int, open: Char, close: Char): (String, Int) = {
    if (startIndex >= s.length || s.charAt(startIndex) != open) {
      throw QasmParseError(s"Expected '$open' at position $startIndex in '$s'")
    }

    var i = startIndex
    var depth = 0
    var inString = false
    var escaped = false

    while (i < s.length) {
      val ch = s.charAt(i)
      if (inString) {
        if (escaped) escaped = false
        else if (ch == '\\') escaped = true
        else if (ch == '"') inString = false
      } else {
        if (ch == '"') inString = true
        else if (ch == open) depth += 1
        else if (ch == close) {
          depth -= 1
          if (depth == 0) {
            return (s.substring(startIndex + 1, i), i + 1)
          }
        }
      }
      i += 1
    }

    throw QasmParseError(s"Unbalanced '$open$close' in '$s'")
  }

  private def splitOnTopLevelArrow(s: String): (String, String) = {
    var paren = 0
    var bracket = 0
    var i = 0
    while (i < s.length - 1) {
      s.charAt(i) match {
        case '(' => paren += 1
        case ')' => paren -= 1
        case '[' => bracket += 1
        case ']' => bracket -= 1
        case '-' if paren == 0 && bracket == 0 && s.charAt(i + 1) == '>' =>
          return (s.substring(0, i), s.substring(i + 2))
        case _ =>
      }
      i += 1
    }
    (s, "")
  }

  private def topLevelEqualsIndex(s: String): Int = {
    var paren = 0
    var bracket = 0
    var i = 0
    while (i < s.length) {
      s.charAt(i) match {
        case '(' => paren += 1
        case ')' => paren -= 1
        case '[' => bracket += 1
        case ']' => bracket -= 1
        case '=' if paren == 0 && bracket == 0 => return i
        case _ =>
      }
      i += 1
    }
    -1
  }

  private def negExpr(expr: String): String = s"-(${expr.trim})"

  private def expectParamCount(name: String, params: List[String], n: Int): List[String] = {
    if (params.length != n) {
      throw QasmParseError(s"$name expects $n parameter(s), got ${params.length}: ${params.mkString("(", ", ", ")")}")
    }
    params
  }

  private def expectArity1(name: String, qs: List[Int]): Int =
    qs match {
      case List(q) => q
      case _ => throw QasmParseError(s"$name expects 1 qubit operand after broadcasting, got ${qs.length}")
    }

  private def expectArity2(name: String, qs: List[Int]): (Int, Int) =
    qs match {
      case List(a, b) => (a, b)
      case _ => throw QasmParseError(s"$name expects 2 qubit operands after broadcasting, got ${qs.length}")
    }

  private def expectArity3(name: String, qs: List[Int]): (Int, Int, Int) =
    qs match {
      case List(a, b, c) => (a, b, c)
      case _ => throw QasmParseError(s"$name expects 3 qubit operands after broadcasting, got ${qs.length}")
    }

  private def maxIndex(qrefs: Iterable[QRef]): Int =
    qrefs.foldLeft(-1) {
      case (m, One(i))   => math.max(m, i)
      case (m, Many(v))  => if (v.isEmpty) m else math.max(m, v.max)
    }

  private def renderModifiers(controls: Int): String =
    if (controls == 1) "ctrl @ " else s"ctrl($controls) @ "
}

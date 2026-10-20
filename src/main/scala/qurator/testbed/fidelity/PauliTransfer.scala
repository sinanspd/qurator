package qurator.testbed.fidelity

import qurator.domain.calibration.{GateTimingKey, gateKey, gateTimingKey}
import qurator.domain.circuit.Gate
import qurator.util.PasqalAnalyticalEstimator

/** Small local matrices only (at most three qubits); never a circuit state vector.
  * Pauli encoding: I=0, X=1, Z=2, Y=3. Local wire 0 is the least significant bit.
  */
private[fidelity] object PauliTransfer {
  final case class Row(outcomes: Array[Int], cumulative: Array[Double]) {
    def sample(random: java.util.Random): Int = {
      if (outcomes.length == 1) outcomes(0)
      else {
        val draw = random.nextDouble()
        var i = 0
        while (i < cumulative.length - 1 && draw >= cumulative(i)) i += 1
        outcomes(i)
      }
    }
  }
  final case class Transfer(rows: Array[Row]) {
    val approximated: Boolean = rows.exists(_.outcomes.length > 1)
  }

  private case class C(re: Double, im: Double = 0.0) {
    def +(b: C): C = C(re + b.re, im + b.im)
    def *(b: C): C = C(re * b.re - im * b.im, re * b.im + im * b.re)
    def conjugate: C = C(re, -im)
    def norm2: Double = re * re + im * im
  }
  private type Matrix = Array[Array[C]]
  private val zero = C(0)
  private def identity(d: Int): Matrix = Array.tabulate(d, d)((r, c) => C(if (r == c) 1 else 0))
  private def multiply(a: Matrix, b: Matrix): Matrix =
    Array.tabulate(a.length, b.length) { (r, c) =>
      var value = zero
      var k = 0
      while (k < a.length) { value = value + a(r)(k) * b(k)(c); k += 1 }
      value
    }
  private def dagger(a: Matrix): Matrix = Array.tabulate(a.length, a.length)((r, c) => a(c)(r).conjugate)
  private def phase(a: Double): C = C(math.cos(a), math.sin(a))

  private def pauli(n: Int, code: Int): Matrix = {
    val d = 1 << n
    val result = Array.fill(d, d)(zero)
    var b = 0
    while (b < d) {
      var out = b
      var factor = C(1)
      var q = 0
      while (q < n) {
        val p = (code >> (2 * q)) & 3
        if ((p & 1) != 0) out ^= 1 << q
        if ((p & 2) != 0) factor = factor * C(if (((b >> q) & 1) == 0) 1 else -1)
        if (p == 3) factor = factor * C(0, 1)
        q += 1
      }
      result(out)(b) = factor
      b += 1
    }
    result
  }

  private def rotation(axis: Int, angle: Double): Matrix = {
    val p = pauli(1, axis)
    Array.tabulate(2, 2) { (r, c) =>
      C(if (r == c) math.cos(angle / 2) else 0) + C(0, -math.sin(angle / 2)) * p(r)(c)
    }
  }
  private def u(theta: Double, phi: Double, lambda: Double): Matrix = {
    val c = math.cos(theta / 2)
    val s = math.sin(theta / 2)
    Array(Array(C(c), phase(lambda) * C(-s)), Array(phase(phi) * C(s), phase(phi + lambda) * C(c)))
  }
  private def controlled(base: Matrix): Matrix =
    Array.tabulate(4, 4) { (r, c) =>
      if ((r & 1) != (c & 1)) zero
      else if ((c & 1) == 0) C(if (r == c) 1 else 0)
      else base(r >> 1)(c >> 1)
    }

  def key(gate: Gate): (GateTimingKey, Int) = gateTimingKey(gate) -> gateKey(gate)._2.size

  def build(gate: Gate): Transfer = {
    val (signature, n) = key(gate)
    require(n >= 1 && n <= 3, s"Pauli evaluator does not support gate $gate")
    val angles = signature.parameters.map { raw =>
      PasqalAnalyticalEstimator.angleRadians(raw).getOrElse(
        throw new IllegalArgumentException(s"Invalid numeric angle '$raw' in $gate"))
    }
    def check(wires: Int, parameters: Int): Unit =
      require(n == wires && angles.size == parameters, s"Invalid operands/parameters for $gate")
    def single(name: String, a: Vector[Double]): Matrix = name match {
      case "X" => pauli(1, 1)
      case "Y" => pauli(1, 3)
      case "Z" => pauli(1, 2)
      case "H" => Array(Array(C(1 / math.sqrt(2)), C(1 / math.sqrt(2))),
        Array(C(1 / math.sqrt(2)), C(-1 / math.sqrt(2))))
      case "ID" | "I" => identity(2)
      case "S" => rotation(2, math.Pi / 2)
      case "SDG" => rotation(2, -math.Pi / 2)
      case "T" => rotation(2, math.Pi / 4)
      case "TDG" => rotation(2, -math.Pi / 4)
      case "SX" => rotation(1, math.Pi / 2)
      case "SXDG" => rotation(1, -math.Pi / 2)
      case "RX" => rotation(1, a(0))
      case "RY" => rotation(3, a(0))
      case "RZ" => rotation(2, a(0))
      case "P" | "PHASE" | "U1" => Array(Array(C(1), zero), Array(zero, phase(a(0))))
      case "U" | "U3" => u(a(0), a(1), a(2))
      case "U2" => u(math.Pi / 2, a(0), a(1))
      // Internal DeviceFitBenchmark convention: equatorial phase in turns, ZZ angle in radians.
      case "GPI" | "GPI2" =>
        val phi = 2 * math.Pi * a(0)
        val theta = if (name == "GPI") math.Pi else math.Pi / 2
        val c = C(math.cos(theta / 2))
        val s = C(0, -math.sin(theta / 2))
        Array(Array(c, s * phase(-phi)), Array(s * phase(phi), c))
      case other => throw new IllegalArgumentException(s"Unsupported Pauli evaluator gate '$other': $gate")
    }
    def parameterCount(name: String): Int = name match {
      case "RX" | "RY" | "RZ" | "P" | "PHASE" | "U1" | "GPI" | "GPI2" => 1
      case "U" | "U3" => 3
      case "U2" => 2
      case _ => 0
    }
    val unitary: Matrix = signature.name match {
      case "SWAP" =>
        check(2, 0)
        Array.tabulate(4, 4)((r, c) => C(if (r == ((c >> 1) | ((c & 1) << 1))) 1 else 0))
      case "CCX" =>
        check(3, 0)
        Array.tabulate(8, 8)((r, c) => C(if (r == (if ((c & 3) == 3) c ^ 4 else c)) 1 else 0))
      case "RXX" | "RYY" | "RZZ" | "ZZ" =>
        check(2, 1)
        val axis = if (signature.name == "RXX") 1 else if (signature.name == "RYY") 3 else 2
        val p = pauli(2, axis | (axis << 2))
        Array.tabulate(4, 4)((r, c) => C(if (r == c) math.cos(angles(0) / 2) else 0) +
          C(0, -math.sin(angles(0) / 2)) * p(r)(c))
      case "CX" | "CY" | "CZ" | "CH" | "CP" | "CRX" | "CRY" | "CRZ" | "CU" =>
        val name = signature.name.drop(1)
        check(2, parameterCount(name))
        controlled(single(name, angles))
      case name =>
        check(1, parameterCount(name))
        single(name, angles)
    }

    // T[Q,P] = |Tr(Q U P U†)/d|². For Clifford U each column is a permutation.
    // For non-Clifford U this deliberately discards off-diagonal Pauli coherences.
    val d = 1 << n
    val basis = Array.tabulate(d * d)(pauli(n, _))
    val adjoint = dagger(unitary)
    Transfer(basis.map { p =>
      val evolved = multiply(multiply(unitary, p), adjoint)
      val probabilities = basis.zipWithIndex.flatMap { case (q, index) =>
        var trace = zero
        var r = 0
        while (r < d) {
          var c = 0
          while (c < d) { trace = trace + q(r)(c) * evolved(c)(r); c += 1 }
          r += 1
        }
        val probability = trace.norm2 / (d * d)
        Option.when(probability > 1e-14)(index -> probability)
      }
      val total = probabilities.map(_._2).sum
      require(math.abs(total - 1) < 1e-9, s"Non-unitary transition for $gate")
      var cumulative = 0.0
      Row(probabilities.map(_._1), probabilities.map { case (_, p) => cumulative += p / total; cumulative })
    })
  }
}

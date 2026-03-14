package qurator.util

import cats.effect.IO
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.{Files, Path}
import fs2.text
import qurator.testbed._
import qurator.domain.circuit._
import qurator.domain.Task._

final case class QuantumTaskLoadWarning(
  file: String,
  message: String
)

final case class QuantumTaskLoadReport(
  tasks: Vector[QuantumTaskSpec],
  warnings: Vector[QuantumTaskLoadWarning]
)

object QuantumTaskLoader {

  final case class Settings(
    folder: Path = Path("mqt"),
    shots: Int = 1000,
    parallelism: Int = math.max(2, Runtime.getRuntime.availableProcessors()),
    parseConfig: Qasm3Parser.ParseConfig = Qasm3Parser.ParseConfig.lenientSkipUnsupported
  )

  private def isQasmFile(path: Path): Boolean = {
    val n = path.fileName.toString.toLowerCase
    n.endsWith(".qasm") || n.endsWith(".qasm3")
  }

  private def circuitNameFrom(path: Path): String = {
    val n = path.fileName.toString
    if (n.toLowerCase.endsWith(".qasm3")) n.dropRight(6)
    else if (n.toLowerCase.endsWith(".qasm")) n.dropRight(5)
    else n
  }

  private def taskDepthOf(c: Circuit): Int = {
    val counts = Array.fill(c.qubits)(0)

    def bump(q: Int): Unit = {
        if (q >= 0 && q < c.qubits) counts(q) += 1
    }

    c.remainingGates.foreach {
        case Measure(q) => bump(q)
        case X(q)        => bump(q)
        case Y(q)        => bump(q)
        case Z(q)        => bump(q)
        case H(q)        => bump(q)
        case S(q)        => bump(q)
        case SDG(q)      => bump(q)
        case T(q)        => bump(q)
        case TDG(q)      => bump(q)
        case SX(q)       => bump(q)
        case SXDG(q)     => bump(q)
        case Id(q)       => bump(q)

        case Phase(_, q) => bump(q)
        case RX(_, q)    => bump(q)
        case RY(_, q)    => bump(q)
        case RZ(_, q)    => bump(q)
        case U(_, _, _, q) =>
        bump(q)
        case U2(_, _, q) =>
        bump(q)
        case U3(_, _, _, q) =>
        bump(q)

        case CX(c0, t0) =>
        bump(c0); bump(t0)
        case CY(c0, t0) =>
        bump(c0); bump(t0)
        case CZ(c0, t0) =>
        bump(c0); bump(t0)
        case CH(c0, t0) =>
        bump(c0); bump(t0)
        case Swap(q1, q2) =>
        bump(q1); bump(q2)

        case CP(c0, _, t0) =>
        bump(c0); bump(t0)
        case CRX(c0, _, t0) =>
        bump(c0); bump(t0)
        case CRY(c0, _, t0) =>
        bump(c0); bump(t0)
        case CRZ(c0, _, t0) =>
        bump(c0); bump(t0)
        case CU(c0, _, _, _, t0) =>
        bump(c0); bump(t0)

        case CCX(c1, c2, t0) =>
        bump(c1); bump(c2); bump(t0)

        case Reset(q) =>
        bump(q)

        case GPhase(_) =>
        () 

        case NamedGate(_, _, qubits) =>
        qubits.distinct.foreach(bump)
    }

    if (counts.isEmpty) 0 else counts.max
    }

  private def readUtf8(path: Path): IO[String] =
    Files[IO]
      .readAll(path)
      .through(text.utf8.decode)
      .compile
      .string

  private def toSpec(c: Circuit, shots: Int): QuantumTaskSpec =
    QuantumTaskSpec(
      circuit = c,
      qubits = TaskQubits(c.qubits),
      shots = TaskShots(shots),
      depth = TaskDepth(taskDepthOf(c))
    )

  private def loadOne(
    path: Path,
    settings: Settings
  ): IO[(Option[QuantumTaskSpec], Vector[QuantumTaskLoadWarning])] = {
    val file = path.fileName.toString
    val name = circuitNameFrom(path)

    readUtf8(path).attempt.flatMap {
      case Left(e) =>
        IO.pure(
          None -> Vector(
            QuantumTaskLoadWarning(file, s"Failed to read file: ${e.getMessage}")
          )
        )

      case Right(qasm) =>
        IO.delay(
          Qasm3Parser.parseWithReport(
            qasm = qasm,
            name = name,
            config = settings.parseConfig
          )
        ).attempt.map {
          case Left(e) =>
            None -> Vector(
              QuantumTaskLoadWarning(file, s"Failed to parse file: ${e.getMessage}")
            )

          case Right(report) =>
            val specOpt =
              if (report.circuit.remainingGates.nonEmpty)
                Some(toSpec(report.circuit, settings.shots))
              else
                None

            val parseWarnings =
              report.warnings.map(w =>
                QuantumTaskLoadWarning(
                  file = file,
                  message = s"${w.message}. Statement: ${w.statement}"
                )
              )

            val emptyWarning =
              if (specOpt.isEmpty)
                Vector(
                  QuantumTaskLoadWarning(
                    file,
                    "File produced no supported gates after lenient parsing; skipped"
                  )
                )
              else Vector.empty

            specOpt -> (parseWarnings ++ emptyWarning)
        }
    }
  }

  def loadReport(
    settings: Settings = Settings()
  ): IO[QuantumTaskLoadReport] = {
    Files[IO].exists(settings.folder).flatMap {
      case false =>
        IO.raiseError(
          new IllegalArgumentException(
            s"Folder does not exist: ${settings.folder}"
          )
        )

      case true =>
        Files[IO].isDirectory(settings.folder).flatMap {
          case false =>
            IO.raiseError(
              new IllegalArgumentException(
                s"Path is not a directory: ${settings.folder}"
              )
            )

          case true =>
            Files[IO]
              .list(settings.folder)
              .filter(isQasmFile)
              .compile
              .toVector
              .map(_.sortBy(_.fileName.toString))
              .flatMap { files =>
                Stream
                  .emits(files)
                  .covary[IO]
                  .parEvalMap(settings.parallelism)(loadOne(_, settings))
                  .compile
                  .toVector
                  .map { results =>
                    val tasks =
                      results.flatMap(_._1)

                    val warnings =
                      results.flatMap(_._2)

                    QuantumTaskLoadReport(
                      tasks = tasks,
                      warnings = warnings
                    )
                  }
              }
        }
    }
  }

  def load(
    settings: Settings = Settings()
  ): IO[Vector[QuantumTaskSpec]] =
    loadReport(settings).map(_.tasks)
}
package qurator.testbed

import cats.effect.{IO, Resource}
import cats.syntax.all._
import io.circe.Json
import io.circe.parser.parse
import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import qurator.domain.calibration.{CanonicalCalibration, gateKey}
import qurator.domain.circuit.Circuit
import qurator.domain.device.Device
import qurator.programs.Scheduler
import qurator.testbed.fidelity.{BenchmarkFidelityEvaluator, DeviceFidelityEvaluation}
import qurator.util.{FidelityEstimator, Qasm3Parser}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** Quantum-only comparison: full Qurator scheduling versus QOS balanced with its analytic predictors. */
object QuantumPolicyBenchmark {
  val shots: Int = 8192 // QOS artifact fixes this value; use it for both policies.
  val preparationNs: Long = 3000000000L

  final case class Settings(
      tasks: Int = 5,
      maxQubits: Int = 5,
      seed: Long = 42L,
      folder: Path = Paths.get("mqt"),
      qosSource: Path = Paths.get("/tmp/qurator-qos-artifacts/scheduler.py"),
      python: String = "/tmp/qurator-qos-artifacts/venv/bin/python",
      targetEstimatedFidelity: Double = 0.9
  ) {
    require(tasks > 0, "Task count must be positive")
    require(maxQubits > 0, "Maximum qubit count must be positive")
    require(targetEstimatedFidelity >= 0.0 && targetEstimatedFidelity <= 1.0,
      "Target estimated fidelity must be between zero and one")
  }

  final case class Candidate(
      device: Device,
      compiled: Circuit,
      calibration: CanonicalCalibration,
      predictedFidelity: Double,
      logFidelity: Double,
      transpileMillis: Long,
      serviceNs: Long,
      predictedServiceNs: Option[Long] = None
  )
  final case class Job(id: Int, circuit: Circuit, candidates: Vector[Candidate])
  final case class Queue(availableNs: Long, jobs: Int) {
    require(availableNs >= 0 && jobs >= 0, "Queue state must be nonnegative")
    def enqueue(serviceNs: Long): Queue = {
      require(serviceNs > 0, "Service duration must be positive")
      Queue(Math.addExact(availableNs, serviceNs), Math.addExact(jobs, 1))
    }
  }
  final case class Assignment(job: Job, candidate: Candidate, queueNs: Long)
  final case class Summary(policy: String, overallFidelity: Double, meanQueueSeconds: Double)
  type Queues = Map[String, Queue]
  type Policy = (Job, Queues) => IO[String]
  final case class QosBridge(predict: Job => IO[Job], policy: Policy)

  // Read declarations before parsing gates, avoiding expensive parsing of over-width circuits.
  // The strict parser checks the complete selected files again; no gates are silently skipped.
  private val comments = """(?s)/\*.*?\*/|//[^\r\n]*""".r
  private val declarations =
    """\bqubit\s*(?:\[\s*(\d+)\s*\])?\s+[A-Za-z_]\w*\s*;|\bqreg\s+[A-Za-z_]\w*\s*\[\s*(\d+)\s*\]\s*;""".r
  private[testbed] def declaredQubits(qasm: String): Int =
    declarations.findAllMatchIn(comments.replaceAllIn(qasm, "")).map { m =>
      Option(m.group(1)).orElse(Option(m.group(2))).fold(1)(_.toInt)
    }.sum

  def loadWorkload(settings: Settings): IO[Vector[Circuit]] = IO.blocking {
    require(Files.isDirectory(settings.folder), s"Missing QASM folder: ${settings.folder}")
    val stream = Files.list(settings.folder)
    val files = try stream.iterator.asScala.filter { p =>
      Files.isRegularFile(p) && (p.toString.endsWith(".qasm") || p.toString.endsWith(".qasm3"))
    }.toVector.sortBy(_.toString) finally stream.close()
    val eligible = files.flatMap { p =>
      val qasm = Files.readString(p)
      val width = declaredQubits(qasm)
      if (width > 0 && width <= settings.maxQubits) Some(p -> qasm) else None
    }
    require(eligible.nonEmpty, s"No declared-width QASM circuits with 1..${settings.maxQubits} qubits in ${settings.folder}")
    val rng = new scala.util.Random(settings.seed)
    val selected = Vector.fill(settings.tasks)(eligible(rng.nextInt(eligible.size)))
    val parsed = selected.distinct.map { case (path, qasm) =>
      val c = Qasm3Parser.parse(qasm, path.toString)
      require(c.qubits > 0 && c.qubits <= settings.maxQubits, s"Parsed width outside limit: $path")
      require(c.remainingGates.nonEmpty, s"Empty circuit: $path")
      path -> c
    }.toMap
    selected.map { case (path, _) => parsed(path) }
  }

  def prepare(circuits: Vector[Circuit], devices: Vector[Device]): IO[Vector[Job]] = {
    val calibrations = BenchmarkDeviceRegistry.defaultCalibrations
    circuits.distinct.traverse { circuit =>
      devices.filter(_.qubits >= circuit.qubits).traverse { device =>
        val raw = calibrations(device.platformId)
        DeviceFidelityEvaluation.compile(circuit, device, raw).adaptError { case e =>
          new IllegalArgumentException(s"Compilation failed for ${circuit.name} on ${device.platformId}: ${e.getMessage}", e)
        }.map { compiled =>
          val cal = FidelityEstimator.normalizeCalibration(raw)
          val prediction = FidelityEstimator.score(compiled, cal)
          require(prediction.pTotal.isFinite && prediction.pTotal >= 0.0 && prediction.pTotal <= 1.0,
            s"Invalid fidelity prediction for ${device.platformId}")
          val perShot = prediction.schedule.get.makespanNs
          require(perShot > 0, s"Missing positive execution duration for ${circuit.name} on ${device.platformId}")
          Candidate(device, compiled, cal, prediction.pTotal, prediction.logPTotal,
            Scheduler.estimatedTranspilationMillis(circuit),
            Math.addExact(preparationNs, Math.multiplyExact(perShot, shots.toLong)))
        }
      }.map { candidates =>
        require(candidates.nonEmpty, s"No device can fit ${circuit.name}")
        circuit -> candidates
      }
    }.map { prepared =>
      val byCircuit = prepared.toMap
      circuits.zipWithIndex.map { case (c, id) => Job(id, c, byCircuit(c)) }
    }
  }

  def initialQueues(devices: Vector[Device], seed: Long): Queues = {
    val rng = new scala.util.Random(seed)
    devices.sortBy(_.platformId).map { d =>
      val count = rng.between(10, 200)
      d.platformId -> Queue(count.toLong * 500000000L, count)
    }.toMap
  }

  val quratorPlacement: Policy = (job, queues) => IO {
    val fleet = job.candidates.map(c => queues(c.device.platformId))
    val meanQueueMillis = fleet.map(_.availableNs / 1000000L).map(_.toDouble).sum / fleet.size
    val meanCount = fleet.map(_.jobs.toDouble).sum / fleet.size
    val scores = job.candidates.map { c =>
      val q = queues(c.device.platformId)
      Scheduler.QuantumDeviceScore(
        c.device,
        Scheduler.getAssignmentCoefficient(c.predictedFidelity, q.jobs, c.transpileMillis,
          meanCount, Some(q.availableNs / 1000000L), Some(meanQueueMillis)),
        c.logFidelity, q.jobs, c.transpileMillis)
    }
    Scheduler.bestQuantumDeviceScore(scores.toList).device.platformId
  }

  /** All arrivals are at t=0; each policy gets its own immutable copy of the initial state.
    * Queue time ends when service starts, excluding execution and offline preparation.
    */
  def replay(jobs: Vector[Job], initial: Queues, policy: Policy): IO[Vector[Assignment]] =
    jobs.foldLeftM((initial, initial, Vector.empty[Assignment])) { case ((forecast, actual, rows), job) =>
      policy(job, forecast).flatMap { id =>
        IO {
          val candidate = job.candidates.find(_.device.platformId == id).getOrElse(
            throw new IllegalArgumentException(s"Policy chose ineligible device '$id' for task ${job.id}"))
          val queue = actual(id)
          (forecast.updated(id, forecast(id).enqueue(candidate.predictedServiceNs.getOrElse(candidate.serviceNs))),
            actual.updated(id, queue.enqueue(candidate.serviceNs)),
            rows :+ Assignment(job, candidate, queue.availableNs))
        }
      }
    }.map(_._3)

  /** One Python process per run; every choice uses the queue owned by replay above. */
  def qos(source: Path, python: String): Resource[IO, QosBridge] = {
    final case class Bridge(process: Process, in: BufferedWriter, out: BufferedReader)
    Resource.make(IO.blocking {
      require(Files.isRegularFile(source),
        s"Missing QOS artifact: $source. Run: python3 scripts/qos_bridge.py --fetch /tmp/qurator-qos-artifacts")
      val process = new ProcessBuilder(python, "-u", "scripts/qos_bridge.py", source.toAbsolutePath.toString)
        .redirectError(ProcessBuilder.Redirect.INHERIT).start()
      Bridge(process, new BufferedWriter(new OutputStreamWriter(process.getOutputStream, UTF_8)),
        new BufferedReader(new InputStreamReader(process.getInputStream, UTF_8)))
    })(b => IO.blocking {
      b.process.destroyForcibly()
      b.in.close()
      b.out.close()
    }).evalMap { b =>
      def read: IO[Json] = IO.interruptible {
        val line = b.out.readLine()
        require(line != null, "QOS bridge exited before returning a result (see stderr)")
        parse(line).fold(throw _, identity)
      }.timeout(30.seconds)
      read.flatMap { ready =>
        IO.fromEither(ready.hcursor.get[Boolean]("ready")).flatMap { ok =>
          IO.raiseUnless(ok)(new IllegalStateException("QOS bridge did not initialize")).as {
            def exchange(request: Json): IO[Json] =
              IO.blocking { b.in.write(request.noSpaces); b.in.newLine(); b.in.flush() } *> read
            val policy: Policy = (job: Job, queues: Queues) => {
              val candidates = job.candidates.map { c =>
                Json.obj("device" -> Json.fromString(c.device.platformId),
                  "capacity" -> Json.fromInt(c.device.qubits),
                  "layout" -> Json.arr(c.compiled.remainingGates.flatMap(gateKey(_)._2).distinct.sorted.map(Json.fromInt): _*),
                  "predictedFidelity" -> Json.fromDoubleOrNull(c.predictedFidelity),
                  "queueNs" -> Json.fromLong(queues(c.device.platformId).availableNs))
              }
              val request = Json.obj("task" -> Json.fromInt(job.id), "candidates" -> Json.arr(candidates: _*))
              exchange(request).flatMap { response =>
                  for {
                    chosen <- IO.fromEither(response.hcursor.get[String]("device"))
                    actualShots <- IO.fromEither(response.hcursor.get[Int]("shots"))
                    _ <- IO.raiseUnless(actualShots == shots)(new IllegalStateException("QOS shot count mismatch"))
                  } yield chosen
                }
            }
            val predict: Job => IO[Job] = job => {
              val candidates = job.candidates.map { c =>
                // Only calibration facts and operation structure cross this boundary, never Qurator predictions.
                val operations = c.compiled.remainingGates.map { gate =>
                  val (name, qs) = gateKey(gate)
                  Json.obj("name" -> Json.fromString(name.toLowerCase),
                    "qubits" -> Json.arr(qs.map(Json.fromInt): _*),
                    "durationNs" -> Json.fromLong(c.calibration.durationNsFor(gate)))
                }
                val gateErrors = (c.calibration.eps1q.toVector.map { case ((_, g), e) => g.toLowerCase -> e } ++
                  c.calibration.eps2q.toVector.map { case ((_, g), e) => g.toLowerCase -> e })
                  .groupMap(_._1)(_._2)
                Json.obj("device" -> Json.fromString(c.device.platformId),
                  "operations" -> Json.arr(operations: _*),
                  "gateErrors" -> Json.obj(gateErrors.toVector.map { case (g, es) =>
                    g -> Json.arr(es.map(Json.fromDoubleOrNull): _*)
                  }: _*),
                  "readoutErrors" -> Json.arr(c.calibration.readoutFidelity.toVector.sortBy(_._1)
                    .map { case (_, f) => Json.fromDoubleOrNull(1.0 - f) }: _*))
              }
              exchange(Json.obj("operation" -> Json.fromString("predict"),
                "candidates" -> Json.arr(candidates: _*))).flatMap { response =>
                IO.fromEither(response.hcursor.get[Vector[Json]]("predictions")).flatMap { predictions =>
                  predictions.traverse { p =>
                    for {
                      id <- IO.fromEither(p.hcursor.get[String]("device"))
                      f <- IO.fromEither(p.hcursor.get[Double]("fidelity"))
                      runtime <- IO.fromEither(p.hcursor.get[Long]("runtimeNs"))
                      c <- IO.fromOption(job.candidates.find(_.device.platformId == id))(
                        new IllegalArgumentException(s"QOS predicted an unknown device $id"))
                      _ <- IO { require(f.isFinite && f >= 0 && f <= 1 && runtime >= 0, "Invalid QOS predictions") }
                    } yield c.copy(predictedFidelity = f, logFidelity = math.log(f),
                      predictedServiceNs = Some(Math.addExact(preparationNs, runtime)))
                  }.flatMap { cs =>
                    IO { require(cs.map(_.device.platformId).sorted == job.candidates.map(_.device.platformId).sorted,
                      "QOS predictions must cover each candidate exactly once") }.as(job.copy(candidates = cs))
                  }
                }
              }
            }
            QosBridge(predict, policy)
          }
        }
      }
    }
  }

  def summarize(policy: String, rows: Vector[Assignment], scores: Map[Candidate, Double]): Summary = {
    require(rows.nonEmpty, "Cannot summarize an empty benchmark")
    Summary(policy, rows.map(r => scores(r.candidate)).sum / rows.size,
      rows.map(_.queueNs.toDouble / 1e9).sum / rows.size)
  }

  def run(settings: Settings): IO[Vector[Summary]] =
    qos(settings.qosSource, settings.python).use { bridge =>
      for {
        circuits <- loadWorkload(settings)
        // These saved IBM profiles contain coupling graphs needed for placement/routing.
        devices = BenchmarkDeviceRegistry.defaultDevices.filter(_.platform == "IBM").sortBy(_.platformId).toVector
        _ <- IO.println(s"Preparing ${circuits.size} quantum tasks (maxQubits=${settings.maxQubits}, " +
          s"seed=${settings.seed}, shots=$shots) across ${devices.size} IBM profiles.")
        jobs <- prepare(circuits, devices)
        queues = initialQueues(devices, settings.seed)
        qosJobs <- jobs.traverse(bridge.predict)
        full <- FullQuantumSchedulerBenchmark.run(circuits, devices, queues, settings.targetEstimatedFidelity)
        quratorRows = full.rows
        qosRows <- replay(qosJobs, queues, bridge.policy)
        evaluator <- IO(BenchmarkFidelityEvaluator.configured)
        scores <- (quratorRows ++ qosRows).map(_.candidate).distinct.traverse { c =>
          IO(c -> evaluator.evaluate(c.compiled, c.calibration).score)
        }.map(_.toMap)
        _ <- IO.println(s"Qurator full scheduler: cutting/merging enabled; ${full.cutInputs} cut inputs, ${full.mergedJobs} merged jobs.")
        _ <- IO.println(s"Fidelity: mean ${evaluator.id} fragment-execution proxy (not reconstructed input fidelity). " +
          "Queue: mean simulated fragment arrival-to-start seconds; merged jobs count once per member.")
      } yield Vector(summarize("Qurator-full", quratorRows, scores),
        summarize("QOS-balanced-native-estimators", qosRows, scores))
    }
}

package qurator.testbed

import cats.effect.{IO, Resource}
import java.nio.file.{Files, Path}
import qurator.domain.circuit._
import qurator.programs.Scheduler
import qurator.testbed.QuantumPolicyBenchmark._
import qurator.util.FidelityEstimator
import weaver.SimpleIOSuite

object QuantumPolicyBenchmarkSuite extends SimpleIOSuite {
  private val devices = BenchmarkDeviceRegistry.defaultDevices.filter(_.platform == "IBM").take(2).toVector
  private val circuit = Circuit(List(X(0), Measure(0)), 1, "one")
  private val cal = FidelityEstimator.normalizeCalibration(BenchmarkDeviceRegistry.defaultCalibrations(devices.head.platformId))
  private def candidate(index: Int, fidelity: Double = 0.9): Candidate =
    Candidate(devices(index), circuit, cal, fidelity, math.log(fidelity), 0L, 3000000000L)
  private def jobs(n: Int): Vector[Job] =
    Vector.tabulate(n)(i => Job(i, circuit, Vector(candidate(0), candidate(1))))
  private val alwaysFirst: Policy = (job, _) => IO.pure(job.candidates.head.device.platformId)

  private def folder(contents: Map[String, String]): Resource[IO, Path] =
    Resource.make(IO.blocking {
      val dir = Files.createTempDirectory("qurator-quantum-policy-test-")
      contents.foreach { case (name, text) => Files.writeString(dir.resolve(name), text) }
      dir
    })(dir => IO.blocking {
      contents.keys.foreach(name => Files.deleteIfExists(dir.resolve(name)))
      Files.deleteIfExists(dir)
      ()
    })

  test("replay counts the initial backlog once and excludes service from queue time") {
    val initial = devices.map(_.platformId -> Queue(5000000000L, 10)).toMap
    for {
      a <- replay(jobs(3), initial, alwaysFirst)
      b <- replay(jobs(3), initial, alwaysFirst)
    } yield expect(a.map(_.queueNs) == Vector(5000000000L, 8000000000L, 11000000000L)) and
      expect(a == b) and expect(initial.values.forall(_.availableNs == 5000000000L))
  }

  test("each next decision sees the updated queue and policies cannot select an unknown device") {
    val initial = devices.map(_.platformId -> Queue(0L, 0)).toMap
    val leastBusy: Policy = (job, qs) =>
      IO.pure(job.candidates.minBy(c => qs(c.device.platformId).availableNs).device.platformId)
    for {
      rows <- replay(jobs(3), initial, leastBusy)
      invalid <- replay(jobs(1), initial, (_, _) => IO.pure("unknown")).attempt
    } yield expect(rows.map(_.candidate.device) == Vector(devices(0), devices(1), devices(0))) and
      expect(rows.map(_.queueNs) == Vector(0L, 0L, 3000000000L)) and expect(invalid.isLeft)
  }

  test("runtime predictions affect policy forecasts without changing measured queue times") {
    val a = candidate(0).copy(predictedServiceNs = Some(1000000000L))
    val work = Vector.tabulate(3)(i => Job(i, circuit, Vector(a)))
    val initial = Map(a.device.platformId -> Queue(5000000000L, 10))
    for {
      seen <- cats.effect.Ref.of[IO, Vector[Long]](Vector.empty)
      policy: Policy = (job, qs) => seen.update(_ :+ qs(a.device.platformId).availableNs).as(a.device.platformId)
      rows <- replay(work, initial, policy)
      forecasts <- seen.get
    } yield expect(forecasts == Vector(5000000000L, 6000000000L, 7000000000L)) and
      expect(rows.map(_.queueNs) == Vector(5000000000L, 8000000000L, 11000000000L))
  }

  test("overall fidelity and queue time are arithmetic task means, including repeated circuits") {
    val j = jobs(3)
    val a = candidate(0)
    val b = candidate(1)
    val rows = Vector(Assignment(j(0), a, 1000000000L), Assignment(j(1), b, 4000000000L),
      Assignment(j(2), a, 1000000000L))
    val result = summarize("test", rows, Map(a -> 0.9, b -> 0.3))
    IO.pure(expect(math.abs(result.overallFidelity - 0.7) < 1e-12) and expect(result.meanQueueSeconds == 2.0))
  }

  test("workload size, width limit and seed are respected before strict gate parsing") {
    folder(Map(
      "small.qasm" -> "OPENQASM 3.0; qubit[2] q; x q[0];",
      "large.qasm" -> "OPENQASM 3.0; qubit[200] q; unsupported q[0];"
    )).use { dir =>
      val s = Settings(tasks = 7, maxQubits = 2, seed = 7, folder = dir)
      for {
        a <- loadWorkload(s)
        b <- loadWorkload(s)
        empty <- loadWorkload(s.copy(maxQubits = 1)).attempt
      } yield expect(a.size == 7) and expect(a.forall(_.qubits == 2)) and expect(a == b) and expect(empty.isLeft)
    }
  }

  test("unsupported selected gates fail instead of changing the circuits being compared") {
    folder(Map("bad.qasm" -> "OPENQASM 3.0; qubit[2] q; unsupported q[0];")).use { dir =>
      loadWorkload(Settings(folder = dir)).attempt.map(r => expect(r.isLeft))
    }
  }

  test("declaration prefilter handles both QASM versions, multiple registers and comments") {
    IO.pure(expect(declaredQubits("// qubit[100] fake;\nqubit[2] q; qubit r; /* qreg fake[50]; */ qreg s[3];") == 6))
  }

  test("shared Qurator coefficient preserves light/heavy load weighting and tie breaks") {
    val light = Scheduler.getAssignmentCoefficient(0.9, 0L, 0L, 0.0, Some(0L), Some(0.0))
    val heavy = Scheduler.getAssignmentCoefficient(0.9, 10000L, 0L, 10000.0,
      Some(3600000L), Some(3600000.0))
    val initial = devices.map(_.platformId -> Queue(5000000000L, 10)).toMap
    val job = Job(0, circuit, Vector(candidate(0, 0.8), candidate(1, 0.9)))
    quratorPlacement(job, initial).map { selected =>
      expect(math.abs(light - 0.72) < 1e-12) and expect(math.abs(heavy + 0.375) < 1e-12) and
        expect(selected == devices(1).platformId)
    }
  }

  test("common preparation compiles for physical topology and uses the shared shot count") {
    prepare(Vector(circuit, circuit), devices.take(1)).map { prepared =>
      val c = prepared.head.candidates.head
      val perShot = FidelityEstimator.score(c.compiled, c.calibration).schedule.get.makespanNs
      expect(prepared.map(_.id) == Vector(0, 1)) and
        expect(prepared(0).candidates == prepared(1).candidates) and
        expect(c.compiled.name.contains("_topology_")) and
        expect(c.serviceNs == preparationNs + perShot * shots)
    }
  }
}

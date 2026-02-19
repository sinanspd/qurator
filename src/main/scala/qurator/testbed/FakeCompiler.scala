package qurator.testbed

import qurator.domain.circuit.Circuit
import qurator.domain.device.Device
import cats.effect._
import cats.syntax.all._
import cats.Applicative

final case class FakeCompiler[F[_] : Applicative](compiled: List[Tuple2[String, Map[String, Circuit]]]) { 
    def compileCircuitFor(device: Device, circuit: Circuit) = {
        compiled.filter(c => circuit.name == c._1).headOption match{
            case None => circuit.pure[F]
            case Some(c) =>  c._2.get(device.platformId).getOrElse(circuit).pure[F]
        }
    }
}
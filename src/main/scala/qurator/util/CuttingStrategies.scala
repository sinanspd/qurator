package qurator.util

import cats.Applicative
import cats.effect.MonadCancelThrow
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import qurator.domain.circuit.Circuit
import qurator.domain.CutQC._
import qurator.domain.device.Device
import qurator.util.Qasm3Parser

object CuttingStrategies {

    def none[F[_]: Applicative](circuit: Circuit, devices: List[Device]): F[List[Circuit]] =
        List(circuit).pure[F]

    // def cutQC[F[_]: MonadCancelThrow: Logger](client: CutQCClient[F])(circuit: Circuit, devices: List[Device]): F[List[Circuit]] = {
    //     Logger[F].info(s"$circuit") *>
    //     client.cut(CutRequest(
    //         circuit = circuit.toQasm,
    //         // TODO: Come up with better heuristics for these constraints
    //         max_cuts = 5, //circuit.qubits,
    //         max_subcircuits = 3,//circuit.qubits,
    //         max_subcircuit_width = devices.map(d => d.qubits).max,
    //         subcircuit_size_imbalance = 2.0
    //     )).map { r =>
    //         r.subcircuits.map(Qasm3Parser.parse(_))
    //     }.handleErrorWith { e =>
    //         Logger[F].warn(s"Using uncut circuit due to error: ${e.getMessage}") *>
    //         List(circuit).pure[F]
    //     }
    // }

}

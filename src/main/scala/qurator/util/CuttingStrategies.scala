package qurator.util

import cats.Applicative
import cats.MonadThrow
import cats.syntax.all._
import qurator.domain.circuit.Circuit
import qurator.domain.calibration.DeviceCalibration
import qurator.domain.cutting._
import qurator.domain.device.Device
import qurator.modules.HttpClients
import qurator.testbed.FakeCompiler

object CuttingStrategies {

    type CuttingStrategy[F[_]] = CuttingRequest => F[CuttingDecision]

    def none[F[_]: Applicative]: CuttingStrategy[F] =
        request => HardwareAwareCuttingPlanner.noCutDecision(request).pure[F]

    def fromSubcircuits[F[_]: cats.Functor](
        name: String
    )(
        split: (Circuit, List[Device]) => F[List[Circuit]]
    ): CuttingStrategy[F] =
        request =>
            split(request.circuit, request.devices).map { subcircuits =>
                HardwareAwareCuttingPlanner.decisionFromSubcircuits(
                    request = request,
                    name = name,
                    subcircuits = subcircuits,
                    explanation = List(s"Adapted legacy subcircuit splitter '$name' to the new cutting-plan interface.")
                )
            }

    def hardwareAware[F[_]: MonadThrow](
        clients: HttpClients[F],
        compiler: FakeCompiler[F],
        config: HardwareAwareCuttingPlanner.Config = HardwareAwareCuttingPlanner.Config()
    ): CuttingStrategy[F] = {
        def fetchCalibration(device: Device): F[DeviceCalibration] =
            clients.providerClient(device.platform).map(_.fetchDeviceCalibration(device.platformId)).getOrElse {
                if (device.platform == "Azure") {
                    clients.azure.fetchDeviceCalibration(device.platformId)
                } else {
                    new RuntimeException(s"No ProviderClient registered for platform=${device.platform}")
                        .raiseError[F, DeviceCalibration]
                }
            }

        request =>
            HardwareAwareCuttingPlanner.plan(
                request = request,
                fetchCalibration = fetchCalibration,
                compileCircuitFor = (device, circuit) => compiler.compileCircuitFor(device, circuit),
                config = config
            )
    }

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

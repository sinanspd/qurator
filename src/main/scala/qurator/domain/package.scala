package qurator

import java.time.LocalDateTime
import qurator.domain.DeviceQueueInformation.DeviceProvider
import qurator.domain.DeviceQueueInformation.QueueType
import cats.{ Eq, Monoid, Show }
import io.circe.{ Decoder, DecodingFailure, Encoder }
import io.circe.generic.extras.semiauto.{ deriveEnumerationDecoder, deriveEnumerationEncoder }
import io.circe.generic.semiauto._
import io.circe.generic.auto._
import io.circe.parser
import io.circe.Error
import qurator.domain.Braket._
import qurator.domain.circuit._
import io.circe.parser.decode

package object domain {
  type QuantumResult = QuantumJobResult
  
  implicit val deviceProviderDecoder: Decoder[DeviceProvider] =
    deriveEnumerationDecoder[DeviceProvider]

  implicit val deviceProviderEncoder: Encoder[DeviceProvider] =
    deriveEnumerationEncoder[DeviceProvider]

  implicit val queueTypeDecoder: Decoder[QueueType] =
    deriveEnumerationDecoder[QueueType]

  implicit val queueTypeEncoder: Encoder[QueueType] =
    deriveEnumerationEncoder[QueueType]

  implicit val dataTimeEq: Eq[LocalDateTime] = Eq.fromUniversalEquals

  def decodeExecutionWindows(deviceCapabilitiesJson: String): Either[Error, List[BraketExecutionWindows]] =
    decode[DeviceCapabilities](deviceCapabilitiesJson).map(_.service.executionWindows)

  def decodeQubitCount(deviceCapabilitiesJson: String): Either[Error, Int] =
    decode[DeviceCapabilities](deviceCapabilitiesJson).map { caps =>
      caps.paradigm.flatMap(p => p.qubitCount.orElse(p.modes)).getOrElse(0)
    }

  def decodeBraketGateSet(deviceCapabilitiesJson: String): List[Gate] =
    parser.parse(deviceCapabilitiesJson).toOption.toList.flatMap { root =>
      val cursor = root.hcursor
      val nativeGateNames =
        cursor.downField("paradigm").downField("nativeGateSet").as[List[String]].toOption.getOrElse(Nil)
      val supportedOperationNames =
        cursor
          .downField("action")
          .downField("braket.ir.openqasm.program")
          .downField("supportedOperations")
          .as[List[String]]
          .toOption
          .getOrElse(Nil)
      val selectedNames =
        if (nativeGateNames.nonEmpty) nativeGateNames else supportedOperationNames

      selectedNames.distinct.flatMap(braketGatePrototype)
    }

  private def braketGatePrototype(rawName: String): Option[Gate] =
    rawName.trim.toLowerCase match {
      case "x" => Some(X(0))
      case "y" => Some(Y(0))
      case "z" => Some(Z(0))
      case "h" => Some(H(0))
      case "s" => Some(S(0))
      case "si" | "sdg" => Some(SDG(0))
      case "t" => Some(T(0))
      case "ti" | "tdg" => Some(TDG(0))
      case "v" | "sx" => Some(SX(0))
      case "vi" | "sxdg" => Some(SXDG(0))
      case "i" | "id" => Some(Id(0))
      case "phaseshift" | "p" | "phase" => Some(Phase("0", 0))
      case "rx" => Some(RX("0", 0))
      case "ry" => Some(RY("0", 0))
      case "rz" => Some(RZ("0", 0))
      case "u" => Some(U("0", "0", "0", 0))
      case "cnot" | "cx" => Some(CX(0, 1))
      case "cy" => Some(CY(0, 1))
      case "cz" => Some(CZ(0, 1))
      case "swap" => Some(Swap(0, 1))
      case "cphaseshift" | "cp" => Some(CP(0, "0", 1))
      case "crx" => Some(CRX(0, "0", 1))
      case "cry" => Some(CRY(0, "0", 1))
      case "crz" | "crotate" => Some(CRZ(0, "0", 1))
      case "ccnot" | "ccx" | "toffoli" => Some(CCX(0, 1, 2))
      case "gpi" => Some(NamedGate("gpi", Vector("0"), Vector(0)))
      case "gpi2" => Some(NamedGate("gpi2", Vector("0"), Vector(0)))
      case "zz" => Some(NamedGate("zz", Vector("0"), Vector(0, 1)))
      case "xx" => Some(NamedGate("xx", Vector("0"), Vector(0, 1)))
      case "yy" => Some(NamedGate("yy", Vector("0"), Vector(0, 1)))
      case "xy" => Some(NamedGate("xy", Vector("0"), Vector(0, 1)))
      case "iswap" => Some(NamedGate("iswap", Vector.empty, Vector(0, 1)))
      case "pswap" => Some(NamedGate("pswap", Vector("0"), Vector(0, 1)))
      case "ecr" => Some(NamedGate("ecr", Vector.empty, Vector(0, 1)))
      case "ms" => Some(NamedGate("ms", Vector("0", "0", "0"), Vector(0, 1)))
      case _ => None
    }

  def deviceExecutionWindows(d: BraketDevice): Either[Error, List[BraketExecutionWindows]] =
    decodeExecutionWindows(d.deviceCapabilities)
}

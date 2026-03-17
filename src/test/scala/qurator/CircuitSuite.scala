package qurator

import qurator.domain.circuit._
import qurator.util.Qasm3Parser
import weaver.SimpleIOSuite

object CircuitSuite extends SimpleIOSuite {

    loggedTest("Circuit round-trips through OpenQASM") { log =>
        val circuit = Circuit(List(
            X(0),
            Y(1),
            Z(2),
            H(0),
            S(1),
            SDG(2),
            T(0),
            TDG(1),
            SX(2),
            SXDG(0),
            Id(1),

            Phase("a", 0),
            RX("b", 1),
            RY("c", 2),
            RZ("d", 0),
            U("e", "f", "g", 1),
            U2("h", "i", 2),
            U3("j", "k", "l", 0),

            CX(0, 1),
            CY(0, 2),
            CZ(1, 0),
            CH(1, 2),
            Swap(2, 0),

            CP(0, "a", 1),
            CRX(0, "b", 2),
            CRY(1, "c", 0),
            CRZ(1, "d", 2),
            CU(2, "e", "f", "g", 0),

            CCX(0, 1, 2),

            Measure(0),
            Reset(1),
            GPhase("a"),

            NamedGate("mygate", Vector("a", "b"), Vector(0, 1, 2))
        ), 3)

        val qasm = circuit.toQasm
        val report = Qasm3Parser.parseWithReport(
            qasm = qasm,
            config = Qasm3Parser.ParseConfig.lenientKeepNamedUnknown
        )

        for {
            _ <- log.info(s"OpenQASM program:\n$qasm")
        } yield
            expect(clue(circuit) == clue(report.circuit)) and
            expect(clue(report.warnings).isEmpty)
    }

}

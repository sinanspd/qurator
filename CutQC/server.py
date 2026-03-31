from argparse import ArgumentParser
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, HTTPServer
import json
import math
from traceback import print_exc

from cutqc.evaluator import modify_subcircuit_instance
from cutqc.main import CutQC
from qiskit import qasm3

class Handler(BaseHTTPRequestHandler):
    error_content_type = "application/json"
    error_message_format = '{"error": {"code": %(code)d, "message": "%(message)s", "explain": "%(explain)s"}}'

    def do_POST(self):
        try:
            request_data = self.rfile.read(int(self.headers["Content-Length"]))
        except:
            return self.send_error(HTTPStatus.LENGTH_REQUIRED)

        try:
            request_data = json.loads(request_data)
        except:
            return self.send_error(HTTPStatus.BAD_REQUEST, message="Invalid JSON", explain="Error parsing JSON input")

        if not isinstance(request_data, dict):
            return self.send_error(HTTPStatus.BAD_REQUEST, message="Incorrect Type", explain="Request data must be JSON object")

        route_handler = {
            "/cut": self.cut,
        }.get(self.path, self.error404)

        try:
            response_data = route_handler(request_data)
        except:
            print_exc()
            return self.send_error(HTTPStatus.INTERNAL_SERVER_ERROR)

        if response_data is not None:
            response_data = json.dumps(response_data).encode()
            self.send_response(HTTPStatus.OK)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(response_data)))
            self.end_headers()
            self.wfile.write(response_data)

    def error404(self, request_data):
        self.send_error(HTTPStatus.NOT_FOUND)

    def cut(self, request_data):
        try:
            circuit = request_data["circuit"]
            max_cuts = request_data["max_cuts"]
            max_subcircuits = request_data["max_subcircuits"]
            max_subcircuit_width = request_data["max_subcircuit_width"]
            subcircuit_size_imbalance = request_data["subcircuit_size_imbalance"]
        except KeyError as e:
            return self.send_error(HTTPStatus.BAD_REQUEST, message="Missing Key", explain=f"JSON object is missing key {e}")

        if not (
            isinstance(circuit, str) and
            isinstance(max_cuts, int) and
            isinstance(max_subcircuit_width, int) and
            isinstance(subcircuit_size_imbalance, (float, int))
        ):
            return self.send_error(HTTPStatus.BAD_REQUEST, message="Incorrect Type", explain="JSON object has key of incorrect type")

        try:
            circuit = qasm3.loads(circuit)
        except:
            return self.send_error(HTTPStatus.BAD_REQUEST, message="Invalid OpenQASM", explain="Error parsing OpenQASM 3 program")

        try:
            cutqc = CutQC(circuit, cutter_constraints={
                "max_cuts": max_cuts,
                "num_subcircuits": range(math.ceil((circuit.num_qubits - 1) / (max_subcircuit_width - 1)), max_subcircuits + 1),
                "max_subcircuit_width": max_subcircuit_width,
                "subcircuit_size_imbalance": subcircuit_size_imbalance,
                "max_subcircuit_cuts": None,
            }, verbose=False)
            cutqc.cut()
        except Exception as e:
            return self.send_error(HTTPStatus.BAD_REQUEST, message="Cutting Error", explain=str(e))

        subcircuits = []
        for i, subcircuit in enumerate(cutqc.cut_solution["subcircuits"]):
            for init, meas in cutqc.subcircuit_instances[i]:
                if "Z" not in meas:
                    subcircuits.append(qasm3.dumps(modify_subcircuit_instance(subcircuit, init, meas)))

        return {"subcircuits": subcircuits}

parser = ArgumentParser()
parser.add_argument("-a", "--address", default="0.0.0.0", help="bind to this address (default: %(default)s)")
parser.add_argument("-p", "--port", type=int, default=8000, help="bind to this port (default: %(default)s)")
args = parser.parse_args()

server = HTTPServer((args.address, args.port), Handler)
print(f"Serving at http://{server.server_address[0]}:{server.server_port}/")
server.serve_forever()

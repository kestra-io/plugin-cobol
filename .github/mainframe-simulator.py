#!/usr/bin/env python3
"""Minimal TCP server set that simulates reachable IBM i service ports."""

import signal
import socket
import threading

HOST = "0.0.0.0"
PORTS = [8470, 8471, 8472, 8473, 8474, 8475, 8476]
shutdown = threading.Event()


def handle_client(connection: socket.socket) -> None:
    with connection:
        try:
            connection.sendall(b"MAINFRAME-SIM\n")
        except OSError:
            return


def serve(port: int) -> None:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as server_socket:
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind((HOST, port))
        server_socket.listen()
        server_socket.settimeout(1)

        while not shutdown.is_set():
            try:
                connection, _ = server_socket.accept()
            except socket.timeout:
                continue
            except OSError:
                break

            worker = threading.Thread(target=handle_client, args=(connection,), daemon=True)
            worker.start()


def stop(*_args) -> None:
    shutdown.set()


def main() -> None:
    signal.signal(signal.SIGTERM, stop)
    signal.signal(signal.SIGINT, stop)

    threads = []
    for port in PORTS:
        thread = threading.Thread(target=serve, args=(port,), daemon=True)
        thread.start()
        threads.append(thread)

    while not shutdown.is_set():
        shutdown.wait(timeout=1)


if __name__ == "__main__":
    main()

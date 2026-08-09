#!/usr/bin/env python3
"""
interactive.py - run the monitor ROM interactively in the simulator

Your terminal becomes the SXB's USB console: keystrokes go to the
emulated FT245 FIFO, monitor output comes back.  Launch with `make sim`.

Keys:
  Ctrl-]        quit the simulator
  Enter         while the CPU is parked in WAI (after X), NMI back in
  ESC / Ctrl-C  the monitor's own stop key (passed through)

Usage: interactive.py [rom.bin [rom.labels]]
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)

from sxb_sim import SXBSim  # noqa: E402

REPO_ROOT = os.path.dirname(HERE)
QUIT_KEY = 0x1D  # Ctrl-]


def write(text):
    sys.stdout.write(text)
    sys.stdout.flush()


def run_tty(sim):
    import termios
    import tty

    fd = sys.stdin.fileno()
    saved = termios.tcgetattr(fd)
    tty.setraw(fd)  # raw: ESC/Ctrl-C reach the monitor; output sends CRLF
    try:
        while True:
            state = sim.run_until_blocked()
            write(sim.take_output())
            if state == "wai":
                write("\r\n[CPU in WAI - Enter: NMI into monitor, Ctrl-]: quit]\r\n")
                key = os.read(fd, 1)[0]
                if key == QUIT_KEY:
                    return
                sim.nmi()
                continue
            key = os.read(fd, 1)[0]
            if key == QUIT_KEY:
                return
            if key == 0x0A:  # some terminals send LF for Enter
                key = 0x0D
            sim.in_queue.append(key)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, saved)
        write("\n")


def run_script(sim):
    """stdin is not a tty: feed it line by line (useful for piping)."""
    for line in sys.stdin.read().splitlines():
        if sim.state == "wai":
            sim.nmi()
        write(sim.command(line))
    write("\n")


def main():
    rom = sys.argv[1] if len(sys.argv) > 1 else os.path.join(
        REPO_ROOT, "build", "rom.bin"
    )
    labels = sys.argv[2] if len(sys.argv) > 2 else os.path.join(
        REPO_ROOT, "build", "rom.labels"
    )
    sim = SXBSim(rom, labels)
    write(sim.boot())
    write(sim.enter_monitor())
    if sys.stdin.isatty():
        run_tty(sim)
    else:
        run_script(sim)


if __name__ == "__main__":
    main()

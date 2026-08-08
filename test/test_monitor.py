"""
test_monitor.py - monitor tests running on the simulated W65C02SXB

Each test boots the real ROM image, NMIs into the monitor, and drives it
through the emulated FT245 terminal exactly as a user at a serial console
would.  Run via `make test` (which builds build/rom.bin first).
"""

import os
import unittest

from sxb_sim import SXBSim

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ROM_PATH = os.environ.get("SXB_ROM", os.path.join(REPO_ROOT, "build", "rom.bin"))
LABELS_PATH = os.environ.get(
    "SXB_LABELS", os.path.join(REPO_ROOT, "build", "rom.labels")
)

# Scratch address for memory tests: safely above the monitor's DATA/BSS
# (which start at $0200) and below the C stack at the top of RAM.
SCRATCH = 0x1000


class MonitorTestCase(unittest.TestCase):
    def setUp(self):
        self.sim = SXBSim(ROM_PATH, LABELS_PATH)
        self.banner = self.sim.boot()

    def test_boot_banner(self):
        """Reset runs through init and prints the ready banner."""
        self.assertIn("WDC 65C02SXB READY", self.banner)
        self.assertIn("RAM:", self.banner)

    def test_nmi_enters_monitor(self):
        """NMI lands in the monitor: register dump then a '.' prompt."""
        out = self.sim.enter_monitor()
        self.assertIn("PC  SR AC XR YR SP  NV-BDIZC", out)
        self.assertIn(".", out)

    def test_store_then_peek(self):
        """S writes a byte to memory and P reads it back."""
        self.sim.enter_monitor()

        self.sim.command("S %04X AB" % SCRATCH)
        # Verify the store actually hit simulated memory
        self.assertEqual(self.sim.mem[SCRATCH], 0xAB)

        out = self.sim.command("P %04X" % SCRATCH)
        self.assertIn("%04X AB" % SCRATCH, out)

    def test_store_overwrites(self):
        """A second store to the same address replaces the value."""
        self.sim.enter_monitor()

        self.sim.command("S %04X 5A" % SCRATCH)
        self.sim.command("S %04X C3" % SCRATCH)
        self.assertEqual(self.sim.mem[SCRATCH], 0xC3)

        out = self.sim.command("P %04X" % SCRATCH)
        self.assertIn("%04X C3" % SCRATCH, out)


if __name__ == "__main__":
    unittest.main()

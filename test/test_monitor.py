"""
test_monitor.py - monitor tests running on the simulated W65C02SXB

One test per documented monitor operation (README table plus the :, ;,
and ' commands documented in the built-in help).  Each test boots the
real ROM image, NMIs into the monitor, and drives it through the
emulated FT245 terminal exactly as a user at a serial console would.
Run via `make test` (which builds build/rom.bin first).

Tests marked @unittest.expectedFailure document real bugs found in the
monitor: they assert the *intended* behavior and currently fail.  When a
bug is fixed the test flips to "unexpected success" so the marker must
be removed - the suite stays an accurate bug ledger either way.
"""

import os
import unittest

from sxb_sim import SXBSim

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ROM_PATH = os.environ.get("SXB_ROM", os.path.join(REPO_ROOT, "build", "rom.bin"))
LABELS_PATH = os.environ.get(
    "SXB_LABELS", os.path.join(REPO_ROOT, "build", "rom.labels")
)

# Scratch addresses: safely above the monitor's DATA/BSS (which end
# around $0A85) and below the C stack at the top of RAM.
SCRATCH = 0x1000


class MonitorTestCase(unittest.TestCase):
    def setUp(self):
        self.sim = SXBSim(ROM_PATH, LABELS_PATH)
        self.banner = self.sim.boot()
        self.sim.enter_monitor()

    # ------------------------------------------------------------------
    # helpers
    # ------------------------------------------------------------------
    def poke(self, addr, data):
        for i, b in enumerate(data):
            self.sim.mem[addr + i] = b

    def bytes_at(self, addr, count):
        return bytes(self.sim.mem[addr : addr + count])

    def set_saved_pc(self, addr):
        """Point the monitor's saved user PC at addr (as the ; command
        would) by writing the register-save area directly."""
        self.sim.mem[self.sim.labels["mon_pclsave"]] = addr & 0xFF
        self.sim.mem[self.sim.labels["mon_pchsave"]] = addr >> 8

    # ------------------------------------------------------------------
    # boot / entry
    # ------------------------------------------------------------------
    def test_boot_banner(self):
        """Reset runs through init and prints the ready banner."""
        self.assertIn("WDC 65C02SXB READY", self.banner)
        self.assertIn("RAM:", self.banner)

    def test_nmi_enters_monitor(self):
        """NMI lands in the monitor: register dump then a '.' prompt."""
        sim = SXBSim(ROM_PATH, LABELS_PATH)
        sim.boot()
        out = sim.enter_monitor()
        self.assertIn("PC  SR AC XR YR SP  NV-BDIZC", out)
        self.assertIn(".", out)

    # ------------------------------------------------------------------
    # A - assemble
    # ------------------------------------------------------------------
    def test_assemble(self):
        """A assembles typed instructions to memory; empty line exits and
        disassembles the result."""
        self.sim.command("A 1400")
        out = self.sim.command("LDA #$42")
        self.assertIn("A9 42", out)
        out = self.sim.command("STA $1100")
        self.assertIn("8D 00 11", out)
        self.sim.command("RTS")
        out = self.sim.command("")  # empty line ends assembly
        self.assertEqual(self.bytes_at(0x1400, 6), bytes.fromhex("A9428D001160"))
        # exit re-disassembles what was entered
        self.assertIn("LDA", out)
        self.assertIn("RTS", out)

    # ------------------------------------------------------------------
    # C - convert (relocate + copy)
    # ------------------------------------------------------------------
    def test_convert(self):
        """C relocates address references, then copies the block."""
        # 1000: JMP $1003 / 1003: RTS
        self.poke(0x1000, bytes.fromhex("4C031060"))
        self.sim.command("C 1000 1004 2000 1000 1004")
        # the copy at 2000 must point into itself, not back at 1000
        self.assertEqual(self.bytes_at(0x2000, 4), bytes.fromhex("4C032060"))

    # ------------------------------------------------------------------
    # D - disassemble
    # ------------------------------------------------------------------
    def test_disassemble(self):
        """D lists instructions with raw bytes and mnemonics."""
        self.poke(0x1300, bytes.fromhex("A9428D00114C0013"))
        out = self.sim.command("D 1300 1307")
        self.assertIn("LDA #42", out)
        self.assertIn("STA 1100", out)
        self.assertIn("JMP 1300", out)
        self.assertIn("---", out)  # separator after JMP

    # ------------------------------------------------------------------
    # F - find byte sequence
    # ------------------------------------------------------------------
    def test_find_byte_sequence(self):
        """F matches the full byte sequence, not just its first byte."""
        self.poke(0x1200, [0x00] * 0x21)
        self.poke(0x1210, [0xDE, 0xAD])
        self.poke(0x1218, [0xDE, 0x01])  # first byte only - must NOT match
        out = self.sim.command("F DE AD, 1200 1220")
        self.assertIn("1210", out)
        self.assertNotIn("1218", out)

    def test_find_single_byte(self):
        """F with a one-byte pattern reports every occurrence."""
        self.poke(0x1200, [0x00] * 0x21)
        self.poke(0x1204, [0x5C])
        self.poke(0x1213, [0x5C])
        out = self.sim.command("F 5C, 1200 1220")
        self.assertIn("1204", out)
        self.assertIn("1213", out)

    # ------------------------------------------------------------------
    # FA / FZ / FI / FR / FT - find in opcode arguments
    #
    # Both the README's spaced form ('FA aaaa, ...') and the compact
    # form ('FAaaaa, ...') are accepted.
    # ------------------------------------------------------------------
    def test_find_absolute_address(self):
        """FA finds instructions whose absolute operand matches."""
        self.poke(0x1500, [0xEA] * 8)
        self.poke(0x1502, bytes.fromhex("AD3412"))  # LDA $1234
        out = self.sim.command("FA 1234, 1500 1507")
        self.assertIn("1502", out)
        self.assertIn("LDA", out)

    def test_find_compact_syntax(self):
        """FA also accepts its argument without a separating space."""
        self.poke(0x1500, [0xEA] * 8)
        self.poke(0x1502, bytes.fromhex("AD3412"))  # LDA $1234
        out = self.sim.command("FA1234, 1500 1507")
        self.assertIn("1502", out)

    def test_find_zero_page_address(self):
        """FZ finds instructions with a matching zero-page operand."""
        self.poke(0x1500, [0xEA] * 8)
        self.poke(0x1503, bytes.fromhex("A542"))  # LDA $42
        out = self.sim.command("FZ 42, 1500 1507")
        self.assertIn("1503", out)
        self.assertIn("LDA", out)

    def test_find_immediate(self):
        """FI finds instructions with a matching immediate operand."""
        self.poke(0x1500, [0xEA] * 8)
        self.poke(0x1504, bytes.fromhex("A937"))  # LDA #$37
        out = self.sim.command("FI 37, 1500 1507")
        self.assertIn("1504", out)
        self.assertIn("LDA", out)

    def test_find_relative_target(self):
        """FR finds branches that target the given address."""
        self.poke(0x1500, [0xEA] * 16)
        self.poke(0x1504, bytes.fromhex("F008"))  # BEQ $150E
        out = self.sim.command("FR 150E, 1500 150D")
        self.assertIn("1504", out)
        self.assertIn("BEQ", out)

    def test_find_table(self):
        """FT reports bytes that do not decode as instructions."""
        self.poke(0x1520, [0xEA] * 8)
        self.poke(0x1524, [0x07, 0x07])  # RMB0 - not in the opcode tables
        out = self.sim.command("FT 1520 1527")
        self.assertIn("***", out)
        self.assertIn("1524", out)

    # ------------------------------------------------------------------
    # G - go
    # ------------------------------------------------------------------
    def test_go(self):
        """G runs code; BRK returns to the monitor with registers saved."""
        # LDA #$42 / STA $1100 / BRK
        self.poke(0x1000, bytes.fromhex("A9428D001100"))
        out = self.sim.command("G 1000")
        self.assertEqual(self.sim.mem[0x1100], 0x42)
        # BRK re-enters the monitor and shows registers, with AC=42
        self.assertIn("PC  SR AC XR YR SP", out)
        self.assertIn(" 42 ", out)

    def test_go_indirect(self):
        """G (xxxx) jumps through a vector."""
        self.poke(0x1000, bytes.fromhex("A9558D001100"))
        self.poke(0x1200, [0x00, 0x10])  # vector -> $1000
        self.sim.command("G (1200)")
        self.assertEqual(self.sim.mem[0x1100], 0x55)

    def test_go_no_address(self):
        """Bare G runs from the saved PC instead of erroring."""
        self.poke(0x1000, bytes.fromhex("A9438D001100"))
        self.set_saved_pc(0x1000)
        out = self.sim.command("G")
        self.assertNotIn("?", out)
        self.assertEqual(self.sim.mem[0x1100], 0x43)
        self.assertIn("PC  SR AC XR YR SP", out)  # BRK back into monitor

    # ------------------------------------------------------------------
    # H - help
    # ------------------------------------------------------------------
    def test_help(self):
        """H prints the full command summary (across output pauses)."""
        out = self.sim.command("H")
        self.assertIn("A xxxx - Assemble", out)
        self.assertIn("TW (xxxx) - Trace walk", out)
        self.assertIn("%bbbbbbbb - convert BIN", out)  # last help line

    # ------------------------------------------------------------------
    # K - ASCII dump
    # ------------------------------------------------------------------
    def test_ascii_dump(self):
        """K shows memory as ASCII."""
        self.poke(0x1600, b"HELLO")
        out = self.sim.command("K 1600 1604")
        self.assertIn("1600", out)
        self.assertIn("HELLO", out)

    # ------------------------------------------------------------------
    # L - Intel HEX load
    # ------------------------------------------------------------------
    def test_load_intel_hex(self):
        """L loads Intel HEX records into memory."""
        self.sim.command("L")  # loader now waits for records
        out = self.sim.command(":0516000048454C4C4F71")
        self.assertIn("+", out)  # checksum OK acknowledgement
        self.sim.command(":00000001FF")  # EOF record returns to prompt
        self.assertEqual(self.bytes_at(0x1600, 5), b"HELLO")

    def test_load_intel_hex_bad_checksum(self):
        """L rejects a record with a bad checksum."""
        self.sim.command("L")
        out = self.sim.command(":0516000048454C4C4F72")  # checksum off by one
        self.assertIn("C", out)  # checksum error flag
        self.assertIn("?", out)

    # ------------------------------------------------------------------
    # M / MS / MT - memory dump, size, test
    # ------------------------------------------------------------------
    def test_hex_dump(self):
        """M dumps memory as hex with ASCII alongside."""
        data = bytes(range(0x41, 0x51))  # 'A'..'P'
        self.poke(0x1700, data)
        out = self.sim.command("M 1700 170F")
        self.assertIn(":1700", out)
        for b in data:
            self.assertIn("%02X" % b, out)
        self.assertIn("ABCDEFG", out)  # ASCII gutter
        # compact form (no space after M) dumps the same line
        out = self.sim.command("M1700 170F")
        self.assertIn(":1700", out)
        self.assertIn("ABCDEFG", out)

    def test_memory_size(self):
        """MS scans upward from $0100 and prints where RAM stops
        responding.  In the simulator plain RAM extends to the first
        emulated VIA register at $7FE0 (on hardware the scan would stop
        at whatever I/O reads back differently)."""
        out = self.sim.command("MS")
        self.assertIn("7FE0", out)

    def test_memory_test(self):
        """MT walks patterns over a range, printing + per pass."""
        out = self.sim.command("MT 1800 18FF")
        self.assertEqual(out.count("+"), 4)  # 4 patterns, 1 repetition
        out = self.sim.command("MT 1800 18FF 02")
        self.assertEqual(out.count("+"), 8)

    def test_memory_test_covers_full_range(self):
        """MT includes the end address, like O/W/M.  (The last pattern,
        $FF, stays in every tested cell, so coverage is observable.)"""
        self.sim.command("MT 1800 18FF")
        self.assertEqual(self.sim.mem[0x18FE], 0xFF)
        self.assertEqual(self.sim.mem[0x18FF], 0xFF)
        self.assertEqual(self.sim.mem[0x1900], 0x00)  # not past the end

    # ------------------------------------------------------------------
    # O - fill
    # ------------------------------------------------------------------
    def test_fill(self):
        """O fills an inclusive range with a byte."""
        self.sim.command("O 1900 190F 5A")
        self.assertEqual(self.bytes_at(0x1900, 16), b"\x5a" * 16)
        self.assertEqual(self.sim.mem[0x1910], 0x00)  # not past the end

    # ------------------------------------------------------------------
    # P / S - peek / store
    # ------------------------------------------------------------------
    def test_store_then_peek(self):
        """S writes a byte to memory and P reads it back."""
        self.sim.command("S %04X AB" % SCRATCH)
        self.assertEqual(self.sim.mem[SCRATCH], 0xAB)
        out = self.sim.command("P %04X" % SCRATCH)
        self.assertIn("%04X AB" % SCRATCH, out)

    def test_store_overwrites(self):
        """A second store to the same address replaces the value."""
        self.sim.command("S %04X 5A" % SCRATCH)
        self.sim.command("S %04X C3" % SCRATCH)
        self.assertEqual(self.sim.mem[SCRATCH], 0xC3)

    # ------------------------------------------------------------------
    # R - registers
    # ------------------------------------------------------------------
    def test_registers(self):
        """R displays the saved register file."""
        out = self.sim.command("R")
        self.assertIn("PC  SR AC XR YR SP  NV-BDIZC", out)
        self.assertIn(";", out)

    # ------------------------------------------------------------------
    # TW / TB / TQ / TS - trace
    # ------------------------------------------------------------------
    def test_trace_walk(self):
        """TW single-steps: shows state, advances one instruction per
        keypress, ESC stops."""
        # LDA #$42 / STA $1100 / BRK
        self.poke(0x1A30, bytes.fromhex("A9428D001100"))
        out = self.sim.command("TW 1A30", auto_page=False)
        self.assertEqual(self.sim.state, "pause")  # waiting for step key
        self.assertIn("1A30", out)
        self.assertIn("LDA", out)  # about to execute LDA #$42
        out = self.sim.press_key("\r")  # step over LDA
        self.assertIn("42", out)   # AC now 42
        self.assertIn("STA", out)  # next instruction shown
        self.assertEqual(self.sim.mem[0x1100], 0x00)  # STA not yet run
        out = self.sim.press_key("\x1b")  # ESC ends the walk
        self.assertEqual(self.sim.state, "input")
        self.assertIn("PC  SR AC XR YR SP", out)

    def test_trace_stop(self):
        """TS runs from the saved PC until it reaches an address."""
        # LDA #$42 / STA $1100 / NOP / NOP / RTS
        self.poke(0x1A00, bytes.fromhex("A9428D0011EAEA60"))
        self.set_saved_pc(0x1A00)
        out = self.sim.command("TS 1A06")
        self.assertEqual(self.sim.mem[0x1100], 0x42)  # code actually ran
        self.assertIn(";1A06", out)                   # stopped at target
        self.assertEqual(self.sim.mem[0x1A06], 0xEA)  # BRK removed again

    def test_trace_break(self):
        """TB stops at a breakpoint after N hits."""
        # 1A10: LDA #$42 / 1A12: INC $1100 / 1A15: JMP $1A12
        self.poke(0x1A10, bytes.fromhex("A942EE00114C121A"))
        self.set_saved_pc(0x1A10)
        out = self.sim.command("TB 1A12 02")
        self.assertIn(";1A12", out)                   # stopped at breakpoint
        self.assertEqual(self.sim.mem[0x1100], 1)     # looped exactly once
        self.assertEqual(self.sim.mem[0x1A12], 0xEE)  # BRK removed again

    def test_trace_quick(self):
        """TQ re-runs to the breakpoint remembered from TS/TB, from an
        optional start address (default: the current PC, stepping over
        the breakpoint it is sitting on)."""
        # 1A20: LDA #$37 / 1A22: INC $1100 / 1A25: JMP $1A22
        self.poke(0x1A20, bytes.fromhex("A937EE00114C221A"))
        self.set_saved_pc(0x1A20)
        out = self.sim.command("TB 1A25 01")  # set breakpoint, run to it
        self.assertIn(";1A25", out)
        self.assertEqual(self.sim.mem[0x1100], 1)
        out = self.sim.command("TQ")  # continue from the stop, once around
        self.assertIn(";1A25", out)
        self.assertEqual(self.sim.mem[0x1100], 2)
        out = self.sim.command("TQ 1A22")  # explicit start address
        self.assertIn(";1A25", out)
        self.assertEqual(self.sim.mem[0x1100], 3)
        self.assertEqual(self.sim.mem[0x1A25], 0x4C)  # BRK removed again

    def test_trace_quick_requires_breakpoint(self):
        """TQ errors when no TS/TB breakpoint has been set yet."""
        self.poke(0x1A20, bytes.fromhex("A937EA"))
        out = self.sim.command("TQ 1A20")
        self.assertIn("?", out)

    def test_trace_walk_no_address(self):
        """Bare TW (with or without trailing whitespace) steps one
        instruction from the saved PC, then keeps walking."""
        # LDA #$42 / STA $1100 / BRK
        self.poke(0x1A30, bytes.fromhex("A9428D001100"))
        self.set_saved_pc(0x1A30)
        out = self.sim.command("TW ", auto_page=False)
        self.assertEqual(self.sim.state, "pause")     # walking, wants a key
        self.assertIn("1A32", out)                    # PC after the LDA step
        self.assertIn("STA", out)                     # next instruction shown
        self.assertEqual(self.sim.mem[0x1100], 0x00)  # STA not yet run
        out = self.sim.press_key("\x1b")              # ESC ends the walk
        self.assertEqual(self.sim.state, "input")
        self.assertIn(" 42 ", out)                    # A holds the LDA result

    # ------------------------------------------------------------------
    # V - relocate address references
    # ------------------------------------------------------------------
    def test_relocate(self):
        """V rewrites absolute operands that fall inside a range."""
        self.poke(0x1000, bytes.fromhex("4C0310EA"))  # JMP $1003 / NOP
        self.sim.command("V 1000 1004 3000 1000 1003")
        self.assertEqual(self.bytes_at(0x1000, 3), bytes.fromhex("4C0330"))

    # ------------------------------------------------------------------
    # W - copy memory
    # ------------------------------------------------------------------
    def test_copy(self):
        """W copies an inclusive range."""
        data = bytes(range(0x10, 0x20))
        self.poke(0x1B00, data)
        self.sim.command("W 1B00 1B0F 2B00")
        self.assertEqual(self.bytes_at(0x2B00, 16), data)
        self.assertEqual(self.sim.mem[0x2B10], 0x00)  # nothing past the end

    # ------------------------------------------------------------------
    # X - exit monitor
    # ------------------------------------------------------------------
    def test_exit_monitor(self):
        """X returns to the interrupted code (the ROM's WAI loop), and a
        new NMI re-enters the monitor."""
        self.sim.command("X")
        self.assertEqual(self.sim.state, "wai")
        out = self.sim.enter_monitor()
        self.assertIn("PC  SR AC XR YR SP", out)

    # ------------------------------------------------------------------
    # ? - expression evaluation
    # ------------------------------------------------------------------
    def test_evaluate(self):
        """? evaluates +, -, *, / and prints hex/binary/decimal."""
        out = self.sim.command("? 0002+0003")
        self.assertIn("05 00000101 5", out)
        out = self.sim.command("? 0002 + 0003")  # spaces around operator
        self.assertIn("05 00000101 5", out)
        out = self.sim.command("? 0009-0004")
        self.assertIn("05 00000101 5", out)
        out = self.sim.command("? 0006*0007")
        self.assertIn("2A 00101010 42", out)
        out = self.sim.command("? 0038/0007")
        self.assertIn("08 00001000 8", out)

    # ------------------------------------------------------------------
    # = - compare memory
    # ------------------------------------------------------------------
    def test_compare(self):
        """= reports the first address where two blocks differ."""
        data = bytes(range(0x30, 0x40))
        self.poke(0x1C00, data)
        self.poke(0x2C00, data)
        self.sim.mem[0x2C08] ^= 0xFF
        out = self.sim.command("= 1C00 2C00")
        self.assertIn("1C08", out)

    # ------------------------------------------------------------------
    # # / $ / % - number base conversion
    # ------------------------------------------------------------------
    def test_decimal_conversion(self):
        """# converts decimal to hex and binary."""
        out = self.sim.command("#255")
        self.assertIn("FF 11111111 255", out)
        out = self.sim.command("#300")
        self.assertIn("012C", out)
        self.assertIn("300", out)

    def test_hex_conversion(self):
        """$ converts hex to decimal and binary."""
        out = self.sim.command("$FF")
        self.assertIn("FF 11111111 255", out)
        out = self.sim.command("$1234")
        self.assertIn("4660", out)

    def test_binary_conversion(self):
        """% converts binary to decimal and hex."""
        out = self.sim.command("%10100101")
        self.assertIn("A5 10100101 165", out)

    # ------------------------------------------------------------------
    # : / ; / ' - documented in the built-in help
    # ------------------------------------------------------------------
    def test_colon_edit(self):
        """: writes a row of hex bytes to memory."""
        self.sim.command(": 1D00 AA BB CC")
        self.assertEqual(self.bytes_at(0x1D00, 3), bytes.fromhex("AABBCC"))

    def test_semicolon_set_registers(self):
        """; sets PC SR AC XR YR SP, visible in R (any spacing)."""
        self.sim.command("; 1234  A1 42  11 22 F0")  # uneven spacing is fine
        out = self.sim.command("R")
        self.assertIn(";1234 A1 42 11 22 F0", out)
        self.assertIn("10100001", out)  # SR echoed in binary

    def test_tick_ascii_entry(self):
        """' stores typed characters into memory."""
        self.sim.command("' 1E00 HELLO")
        self.assertEqual(self.bytes_at(0x1E00, 5), b"HELLO")


if __name__ == "__main__":
    unittest.main()

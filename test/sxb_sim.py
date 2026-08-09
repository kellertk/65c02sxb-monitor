"""
sxb_sim.py - simulated W65C02SXB for monitor unit tests

Runs the real ROM image (build/rom.bin) on py65's 65C02 core (the SXB only
uses a W65C02, so the simulation always uses py65.devices.mpu65c02) and
emulates just enough of the board for the monitor to run:

  $0000-$7EFF  RAM
  $7F00-$7FFF  I/O space (VIA2 at $7FE0 hooked, everything else reads 0)
  $8000-$FFFF  ROM (loaded from rom.bin; writes are discarded, like real ROM)

The FT245RL USB FIFO is emulated at the VIA2 register level, mirroring the
handshake protocol in src/usb.s:

  PB bit 0  TXE#  - always driven low (transmit FIFO always has room)
  PB bit 1  RXF#  - low while the host-to-CPU input queue is non-empty
  PB bit 2  WR    - falling edge latches the byte last written to PA
  PB bit 3  RD#   - while low, reads of PA pop the input queue

Blocked-state detection
-----------------------
With the input queue empty, the CPU can be stuck in one of two loops:

  'input' - _usb_getc's blocking wait (getlin/rawgetc): one DDRA write on
            entry, then nothing but RXF# polls.  This is the monitor
            waiting for a command line (or the HEX loader for data).
  'pause' - a _usb_getc_nb polling loop (page_pause's wait-for-keypress,
            trace walk's wait-for-step-key).  Each poll rewrites DDRA
            before testing RXF#.

The DDRA write is the discriminator: `_empty_polls` counts consecutive
empty RXF# polls but resets on DDRA writes, so it only climbs inside the
blocking loop; `_idle_polls` ignores DDRA writes and climbs in both.
Both reset whenever a byte actually moves.  run_until_blocked() reports
'input', 'pause', or 'wai' (the CPU executed WAI - e.g. after the X
command returns to the interrupted main loop).

The boot LED animation (~4M simulated instructions of delay loops) is
patched to an immediate RTS by default so tests start quickly; the patch
address comes from the linker's label file, not a hardcoded offset.
"""

import re
from collections import deque

from py65.devices.mpu65c02 import MPU
from py65.memory import ObservableMemory

ROM_BASE = 0x8000
ROM_SIZE = 0x8000

# VIA2 (W65C22N at $7FE0) registers used by the FT245 driver (src/usb.s)
VIA2 = 0x7FE0
VIA2_PB = VIA2 + 0x0
VIA2_PA = VIA2 + 0x1
VIA2_DDRB = VIA2 + 0x2
VIA2_DDRA = VIA2 + 0x3

# FT245 handshake bits on VIA2 Port B
FT_TXE = 0x01  # TXE# (input)  - low when FT245 transmit FIFO has room
FT_RXF = 0x02  # RXF# (input)  - low when FT245 has received data
FT_WR = 0x04   # WR   (output) - active-high write strobe
FT_RD = 0x08   # RD#  (output) - active-low read strobe

# Consecutive empty RXF# polls before the CPU counts as blocked.
BLOCKED_THRESHOLD = 16   # blocking _usb_getc loop (no DDRA between polls)
PAUSED_THRESHOLD = 64    # _usb_getc_nb polling loop (DDRA write per poll)

DEFAULT_STEP_BUDGET = 2_000_000

ESC = 0x1B
CR = 0x0D
RTS_OPCODE = 0x60


class SimTimeout(Exception):
    """The CPU did not reach the expected state within the step budget."""


class SXBSim:
    def __init__(self, rom_path, labels_path, skip_boot_animation=True):
        with open(rom_path, "rb") as f:
            self.rom = f.read()
        if len(self.rom) != ROM_SIZE:
            raise ValueError(
                "expected a %d byte ROM image, got %d bytes"
                % (ROM_SIZE, len(self.rom))
            )

        self.labels = self._load_labels(labels_path)

        ram = [0x00] * 0x10000
        ram[ROM_BASE:] = list(self.rom)
        if skip_boot_animation:
            ram[self.labels["_boot_animation"]] = RTS_OPCODE

        self.mem = ObservableMemory(subject=ram)
        self.mem.subscribe_to_read([VIA2_PB], self._pb_read)
        self.mem.subscribe_to_read([VIA2_PA], self._pa_read)
        self.mem.subscribe_to_write([VIA2_PB], self._pb_write)
        self.mem.subscribe_to_write([VIA2_PA], self._pa_write)
        self.mem.subscribe_to_write([VIA2_DDRB, VIA2_DDRA], self._ddr_write)
        # ROM is not writable: force stores to keep the original byte.
        # (Write callbacks only fire on actual stores, so this costs
        # nothing on instruction fetches.)
        self.mem.subscribe_to_write(range(ROM_BASE, 0x10000), self._rom_write)

        # FT245 emulation state
        self.in_queue = deque()       # host -> CPU bytes
        self.out_bytes = bytearray()  # CPU -> host bytes
        self._pb_state = 0x00         # last value the CPU wrote to Port B
        self._pa_latch = 0x00         # last value the CPU wrote to Port A
        self._empty_polls = 0         # empty RXF# polls, reset by DDRA writes
        self._idle_polls = 0          # empty RXF# polls, any loop shape

        self.state = None             # last state run_until_blocked returned

        self.mpu = MPU(memory=self.mem, pc=None)  # pc=None: reset via $FFFC
        self.mpu.waiting = False
        self.mpu.reset()

    # ------------------------------------------------------------------
    # VIA2 / FT245 emulation
    # ------------------------------------------------------------------
    def _pb_read(self, address):
        value = self._pb_state & (FT_WR | FT_RD)
        # TXE# always low: transmit FIFO always has room
        if not self.in_queue:
            value |= FT_RXF
            self._empty_polls += 1
            self._idle_polls += 1
        else:
            self._empty_polls = 0
            self._idle_polls = 0
        return value

    def _pb_write(self, address, value):
        # WR falling edge: FT245 latches the byte on Port A
        if (self._pb_state & FT_WR) and not (value & FT_WR):
            self.out_bytes.append(self._pa_latch)
        self._pb_state = value
        self._empty_polls = 0
        self._idle_polls = 0

    def _pa_read(self, address):
        self._empty_polls = 0
        self._idle_polls = 0
        # With RD# asserted the FT245 drives the read FIFO onto the bus
        if not (self._pb_state & FT_RD) and self.in_queue:
            return self.in_queue.popleft()
        return 0xFF  # nothing driving the bus

    def _pa_write(self, address, value):
        self._pa_latch = value
        self._empty_polls = 0
        self._idle_polls = 0

    def _ddr_write(self, address, value):
        # _usb_getc_nb rewrites DDRA on every call; the blocking _usb_getc
        # loop does not.  Only the blocking-loop counter resets here.
        self._empty_polls = 0

    def _rom_write(self, address, value):
        return self.rom[address - ROM_BASE]

    # ------------------------------------------------------------------
    # Execution control
    # ------------------------------------------------------------------
    def run_until_blocked(self, max_steps=DEFAULT_STEP_BUDGET):
        """Step the CPU until it blocks.  Returns and records one of:
        'wai'   - executed WAI (parked outside the monitor)
        'input' - blocking read, wants a line of input (or HEX data)
        'pause' - polling read, wants a single keypress (paged output,
                  trace walk)"""
        self._empty_polls = 0
        self._idle_polls = 0
        mpu = self.mpu
        for _ in range(max_steps):
            mpu.step()
            if mpu.waiting:
                self.state = "wai"
                return self.state
            if self._empty_polls >= BLOCKED_THRESHOLD:
                self.state = "input"
                return self.state
            if self._idle_polls >= PAUSED_THRESHOLD:
                self.state = "pause"
                return self.state
        raise SimTimeout(
            "CPU still busy after %d steps (PC=$%04X)" % (max_steps, mpu.pc)
        )

    def nmi(self):
        """Assert NMI (clears WAI state, vectors through $FFFA)."""
        self.mpu.waiting = False
        self.mpu.nmi()

    def call(self, target, a=0, x=0, y=0, max_steps=DEFAULT_STEP_BUDGET):
        """Call a subroutine directly: JSR `target` (label name or address)
        with the given registers, run until it returns.  Returns the MPU so
        tests can assert on registers and flags."""
        if isinstance(target, str):
            target = self.labels[target]
        sentinel = 0xFFF9  # unused address, never executed
        mpu = self.mpu
        mpu.waiting = False
        mpu.a, mpu.x, mpu.y = a, x, y
        # Push (sentinel - 1); RTS adds one, so the PC lands on sentinel
        ret = (sentinel - 1) & 0xFFFF
        mpu.stPush(ret >> 8)
        mpu.stPush(ret & 0xFF)
        mpu.pc = target
        for _ in range(max_steps):
            mpu.step()
            if mpu.pc == sentinel:
                return mpu
        raise SimTimeout(
            "subroutine at $%04X did not return after %d steps (PC=$%04X)"
            % (target, max_steps, mpu.pc)
        )

    # ------------------------------------------------------------------
    # Terminal-level interface
    # ------------------------------------------------------------------
    def send_keys(self, text):
        """Queue raw characters for the CPU (no CR appended, no run)."""
        for ch in text:
            self.in_queue.append(ord(ch))

    def send_line(self, line):
        """Queue a command line followed by CR (no run)."""
        self.send_keys(line)
        self.in_queue.append(CR)

    def take_output(self):
        """Return and clear everything the monitor sent to the terminal."""
        text = self.out_bytes.decode("latin-1")
        self.out_bytes.clear()
        return text

    def boot(self):
        """Run from reset until the ROM banner is out and main() parks the
        CPU in WAI.  Returns the boot banner text."""
        if self.run_until_blocked() != "wai":
            raise SimTimeout("boot did not reach WAI (blocked on input)")
        return self.take_output()

    def enter_monitor(self):
        """NMI into the monitor; returns the register display + prompt."""
        self.nmi()
        if self.run_until_blocked() != "input":
            raise SimTimeout("monitor did not reach its input loop")
        return self.take_output()

    def command(self, line, auto_page=True, max_pages=100):
        """Type a command line at the monitor prompt and return everything
        it printed, up to the next line-input wait (or WAI, for X/G).

        Paged output pauses for a keypress every screenful; with auto_page
        the harness answers each pause with CR, like a user holding Enter.
        Pass auto_page=False to stop at the first pause instead (needed
        for trace walk, where each keypress single-steps)."""
        self.send_line(line)
        text = ""
        for _ in range(max_pages):
            state = self.run_until_blocked()
            text += self.take_output()
            if state != "pause" or not auto_page:
                return text
            self.send_keys("\r")  # answer the pause, keep going
        raise SimTimeout("paged output did not finish after %d pages" % max_pages)

    def press_key(self, key):
        """Send one keypress to a paused monitor (trace walk step, pager)
        and run until it blocks again.  Returns the new output."""
        self.send_keys(key)
        self.run_until_blocked()
        return self.take_output()

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------
    @staticmethod
    def _load_labels(labels_path):
        """Parse the ld65 -Ln (VICE) label file: 'al 00F80B ._label'."""
        labels = {}
        pattern = re.compile(r"^al\s+([0-9A-Fa-f]+)\s+\.(\S+)")
        with open(labels_path) as f:
            for line in f:
                m = pattern.match(line)
                if m:
                    labels[m.group(2)] = int(m.group(1), 16)
        return labels

# WDC 65C02 SXB Monitor

A machine-language monitor for the
[WDC W65C02SXB](https://wdc65xx.com/Single-Board-Computers/w65c02sxb/)
single-board computer. The stock ROM that ships with these boards comes with a
software that will only work with WDC's TIDE, which I was not able to get to
work on Windows 11. So instead this replaces the stock ROM with a proper
ROM monitor. The board's USB port is connected to an FT245RL USB-parallel FIFO
which the monitor uses for as the debug port.

As a reminder, this machine uses a straightfoward ~32 K RAM + I/O space + 32 K ROM memory map:

```
$0000-$7EFF: RAM
$7F00-$7FFF: I/O space (USB port on VIA2 at $7FE0-$7FFF)
$8000-$FFFF: ROM
```

This was adapted from
[smon6502](https://github.com/dhansel/smon6502/?tab=readme-ov-file#credits),
itself an adaptation of the SMON monitor originally published in the German
magazine *64'er* in 1984. I say adapted because this is not a straight port: 
differences in interrupt handling between the original 6502 and the 65C02
required rewriting the trace and breakpoint code. The orignal source was written
in VASM and used fixed addressing and required the C64's KERNAL ROM.

This is a rewrite specifically for the W65C02SXB, but using the cc65 toolchain
and relocatable asm so it's likely portable to other computers.


## Features

- Memory fill, copy, compare, and dump in hex and ASCII
- Integrated disassembler with 65C02 extended instruction set support
- Search memory by byte sequence, opcode arguments, and addresses references
- Load Intel HEX files from the terminal into memory
- Trace / single-step / breakpoint execution
- Address relocation (convert + copy)
- Immediate evaluation (hex to decimal, binary to hex, etc., integer arithmetic)

## Memory Usage

| Segment    | Bytes | Contents                                  |
|------------|------:|-------------------------------------------|
| `STARTUP`  |    77 | One-time init code                        |
| `CODE`     |  5037 | Program code                              |
| `RODATA`   |  1837 | Strings, tables, help text                |
| `DATA`     |    37 | Initialized data (copied from ROM at startup) |
|            | **6988** | **ROM total** (21% of 32 KB)           |
| `ZEROPAGE` |    75 | Zero-page variables                       |
| `BSS`      |  2141 | Uninitialized variables                   |
|            | **2216** | **RAM total**                          |


## Building

- [cc65](https://cc65.github.io/) toolchain (`ca65`, `ld65`, `cl65`,
  `ar65`)
- [minipro](https://gitlab.com/DavidGriffith/minipro) (for flashing the
  ROM chip)
- GNU Make

| Target      | Description                                         |
|-------------|-----------------------------------------------------|
| `make`      | Build the 32 KB ROM image (`build/rom.bin`)         |
| `make pad`  | Pad to 128 KB for the SST39SF010A (`build/rom.pad`) |
| `make flash_rom` | Pad and flash via minipro                      |
| `make clean`     | Remove build artifacts                          |
| `make rebuild`   | Clean and rebuild                               |

The SBC uses a 128 KB ROM chip, but only the top 32 KB is visible to the CPU.
The ROM image is padded with `$FF` to 128 KB for flashing.

You'll probably need a PLCC-32 to DIP-32 and a PLCC puller to flash the ROM
chip, as well as a programmer. I was never able to get the board's built-in USB
to work, which is why I'm using this in the first place.

## Monitor Commands

The monitor will start on NMI or BRK.

Type `H` at the monitor prompt to display the built-in help.

| Command | Syntax | Description |
|---------|--------|-------------|
| **A** | `A xxxx` | Interactively **a**ssemble starting at `xxxx` (end with `f`, use `Mxx` for labels) |
| **C** | `C xxxx yyyy zzzz aaaa bbbb` | **C**onvert: relocate addresses in `xxxx`-`yyyy` by offset `zzzz`, then copy to `aaaa`-`bbbb` |
| **D** | `D xxxx (yyyy)` | **D**isassemble from `xxxx` (to `yyyy`) |
| **F** | `F aa bb .., xxxx yyyy` | **F**ind byte sequence `aa bb ..` in `xxxx`-`yyyy` |
| **FA** | `FA aaaa, xxxx yyyy` | **F**ind **a**bsolute address `aaaa` in opcodes within `xxxx`-`yyyy` |
| **FR** | `FR aaaa, xxxx yyyy` | **F**ind **r**elative branch target `aaaa` in opcodes within `xxxx`-`yyyy` |
| **FT** | `FT xxxx yyyy` | **F**ind **t**able (non-opcode bytes) in `xxxx`-`yyyy` |
| **FZ** | `FZ aa, xxxx yyyy` | **F**ind **z**ero-page address `aa` in opcodes within `xxxx`-`yyyy` |
| **FI** | `FI aa, xxxx yyyy` | **F**ind **i**mmediate argument `aa` in opcodes within `xxxx`-`yyyy` |
| **G** | `G (xxxx)` | **G**o  - run from `xxxx` (or current PC) |
| **K** | `K xxxx (yyyy)` | ASCII (ass-**k**ey) dump from `xxxx` (to `yyyy`) |
| **L** | `L` | **L**oad Intel HEX data from terminal |
| **M** | `M xxxx (yyyy)` | Hex dump **m**emory from `xxxx` (to `yyyy`) |
| **MS** | `MS` | Print **m**emory **s**ize |
| **MT** | `MT xxxx yyyy (nn)` | **M**emory **t**est `xxxx`-`yyyy`, repeat `nn` times |
| **O** | `O xxxx yyyy aa` | Fill (**o**utput) memory `xxxx`-`yyyy` with byte `aa` |
| **TW** | `TW xxxx` | **T**race **w**alk  - single-step from `xxxx` |
| **TB** | `TB xxxx nn` | **T**race **b**reak  - set breakpoint at `xxxx`, stop after `nn` hits |
| **TQ** | `TQ xxxx` | **T**race **q**uick  - run to breakpoint at `xxxx` |
| **TS** | `TS xxxx` | **T**race **s**top  - run until PC reaches `xxxx` |
| **V** | `V xxxx yyyy zzzz aaaa bbbb` | Relocate (mo**v**e) address references in `aaaa`-`bbbb` from `xxxx`-`yyyy` to `zzzz` |
| **W** | `W xxxx yyyy zzzz` | Copy (**w**rite) memory `xxxx`-`yyyy` to `zzzz` |
| **X** | `X` | E**x**it monitor and return to interrupted code |
| **?** | `? xxxx+yyyy` | Evaluate `xxxx` op `yyyy` (`+` `-` `*` `/`), print result in hex, decimal, and binary |
| **=** | `= xxxx yyyy` | Compare memory at `xxxx` to memory at `yyyy` |
| **#** | `#ddd` | Convert decimal `ddd` to hex and binary |
| **$** | `$xx` | Convert hex `xx` to decimal and binary |
| **%** | `%bbbbbbbb` | Convert binary `bbbbbbbb` to decimal and hex |

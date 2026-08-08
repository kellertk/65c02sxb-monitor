# =============================================================================
# W65C02SXB Project Build System
# =============================================================================
#
# Targets:
#   all       - Build everything (stdlib, rom, padded ROM)
#   rom       - Build the ROM binary (C + asm from src/)
#   stdlib    - Generate the stdlib.lib from cc65
#   pad       - Build the padded 128KB ROM image for external flashing
#   flash_rom - Flash padded ROM to SST39SF010A via minipro
#   test      - Run unit tests in a simulated 65C02 system (needs py65)
#   clean     - Remove all build artifacts
#   rebuild   - Clean + full rebuild
#
# =============================================================================

# Tools
CL65      = cl65
CA65      = ca65
LD65      = ld65
AR65      = ar65
MINIPRO   = minipro

# Directories
SRC_DIR   = src
BUILD_DIR = build
TEST_DIR  = test

# cc65 data location (auto-detected; override with make CC65_DATA_DIR=...)
CC65_DATA_DIR ?= $(firstword $(wildcard /usr/local/share/cc65 /usr/share/cc65))
CC65_LIB_DIR  ?= $(CC65_DATA_DIR)/lib
CC65_INC_DIR  ?= $(CC65_DATA_DIR)/asminc

# Configuration
CFG       = w65c02sxb.cfg

# Build outputs
ROM        = $(BUILD_DIR)/rom.bin
ROM_PADDED = $(BUILD_DIR)/rom-padded.bin
STDLIB     = $(BUILD_DIR)/stdlib.lib

# Flash chip (1 Mbit / 128KB SST39SF010A)
# Only the top 32KB is visible to the CPU, so we pad the lower 96KB with $FF
FLASH_CHIP = SST39SF010A
FLASH_SIZE = 131072
ROM_SIZE   = 32768
PAD_SIZE   = 98304

# Source files
C_SRC     = $(wildcard $(SRC_DIR)/*.c)
ASM_SRC   = $(wildcard $(SRC_DIR)/*.s)
INC_SRC   = $(wildcard $(SRC_DIR)/*.inc)

# =============================================================================
# Main targets
# =============================================================================

.PHONY: all rom stdlib pad flash_rom clean rebuild test

all: $(ROM_PADDED)

rom: $(ROM)

pad: $(ROM_PADDED)

stdlib: $(STDLIB)

# Convenience: clean + full rebuild
rebuild:
	$(MAKE) clean
	$(MAKE) all

# =============================================================================
# Tests
# =============================================================================
# Runs the monitor ROM under a simulated 65C02 system (py65) and drives it
# through the emulated FT245 USB FIFO. Requires python3 and py65
# (pip install py65).
# =============================================================================

test: $(ROM)
	python3 -m unittest discover -s $(TEST_DIR) -v

# =============================================================================
# Standard library generation
# =============================================================================

$(STDLIB): | $(BUILD_DIR)
	cp $(CC65_LIB_DIR)/supervision.lib $(STDLIB)
	$(AR65) d $(STDLIB) crt0.o
	$(AR65) d $(STDLIB) supervision-stdjoy-joy.o

# =============================================================================
# ROM build using cl65 (C + asm from src/)
# Note: The linker config specifies file="rom.bin" for the ROM output,
# so it creates rom.bin in the current directory, then we move it to build/
# =============================================================================

$(ROM): $(C_SRC) $(ASM_SRC) $(INC_SRC) $(CFG) $(STDLIB) | $(BUILD_DIR)
	$(CL65) -t none --no-target-lib -C $(CFG) \
		--asm-include-dir $(SRC_DIR) \
		--asm-include-dir $(CC65_INC_DIR) \
		--obj-path $(BUILD_DIR) \
		-o $(BUILD_DIR)/rom.out \
		-m $(BUILD_DIR)/rom.map \
		-Ln $(BUILD_DIR)/rom.labels \
		$(ASM_SRC) $(C_SRC) $(STDLIB)
	mv rom.bin $(ROM)
	@echo "ROM size: $$(stat -c%s $(ROM)) bytes"

# =============================================================================
# Padded ROM for flash chip
# 96KB $FF padding + 32KB ROM = 128KB total
# =============================================================================

$(ROM_PADDED): $(ROM)
	dd if=/dev/zero bs=1024 count=96 2>/dev/null | tr '\0' '\377' > $(ROM_PADDED)
	cat $(ROM) >> $(ROM_PADDED)
	@echo "Padded ROM size: $$(stat -c%s $(ROM_PADDED)) bytes (expected $(FLASH_SIZE))"

# =============================================================================
# Flash target
# =============================================================================

flash_rom: $(ROM_PADDED)
	$(MINIPRO) -p $(FLASH_CHIP) -w $(ROM_PADDED)

# =============================================================================
# Build directory — created on demand via order-only prerequisites
# =============================================================================

$(BUILD_DIR):
	mkdir -p $@

# =============================================================================
# Clean
# =============================================================================

clean:
	rm -rf $(BUILD_DIR)

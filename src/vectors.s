; ==============================================================================
; vectors.s - 65C02 hardware vectors (NMI, RESET, IRQ)
; ------------------------------------------------------------------------------

.import _init
.import _nmi_int, _irq_int

.segment "VECTORS"
.addr _nmi_int
.addr _init
.addr _irq_int

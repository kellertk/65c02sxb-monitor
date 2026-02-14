; ==============================================================================
; interrupt.s - interrupt handler for WDC 65C02 SXB
; ==============================================================================
; Dispatches IRQ through mon_irq_vec (software vector in BSS).
; Dispatches BRK through mon_brk_vec (software vector in BSS).
; Both handlers receive stack: Y, X, A, SR, PCL, PCH (top->bottom).
; ------------------------------------------------------------------------------
.setcpu "W65C02"

.export _irq_int, _nmi_int

.import mon_irq_vec, mon_brk_vec, mon_nmi

.code

_nmi_int:
    pha                         ; push A
    phx                         ; push X
    phy                         ; push Y
    jmp     mon_nmi             ; enter monitor (saves regs, no PC adjust)

_irq_int:
    pha                         ; push A
    phx                         ; push X
    phy                         ; push Y
    tsx
    lda     $104,x              ; SR on stack (past Y, X, A)
    and     #$10                ; BRK flag?
    bne     @brk
    jmp     (mon_irq_vec)       ; hardware IRQ
@brk:
    jmp     (mon_brk_vec)       ; software BRK

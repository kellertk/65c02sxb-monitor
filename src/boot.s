; ==============================================================================
; boot.s - WDC 65C02 SXB boot animation
; ==============================================================================
; Cycles through the four chip-select lines (EXP0..CS3) to flash their
; active-low CS LEDs in sequence.  Runs 4 passes.
; ------------------------------------------------------------------------------

.setcpu "65C02"

; ------------------------------------------------------------------------------
; Exports
; ------------------------------------------------------------------------------
        .export _boot_animation

; ------------------------------------------------------------------------------
; I/O addresses (from linker config SYMBOLS)
; ------------------------------------------------------------------------------
EXP0    = $7F00
EXP1    = $7F20
EXP2    = $7F40
CS3     = $7F60

; ------------------------------------------------------------------------------
;                             ZEROPAGE
; ------------------------------------------------------------------------------
        .segment "ZEROPAGE"
anim_ctr:       .res 1          ; animation pass counter

        .segment "ONCE"

; ------------------------------------------------------------------------------
; boot animation
; ------------------------------------------------------------------------------
.proc _boot_animation

        lda     #4
        sta     anim_ctr        ; 4 passes

@pass:
        ; --- read EXP0 (pulses /CS for EXP0 LED) ---
        ldy     #0
@d0_y:
        ldx     #0
@d0_x:
        lda     EXP0
        inx
        bne     @d0_x
        iny
        bne     @d0_y

        ; --- read EXP1 ---
        ldy     #0
@d1_y:
        ldx     #0
@d1_x:
        lda     EXP1
        inx
        bne     @d1_x
        iny
        bne     @d1_y

        ; --- read EXP2 ---
        ldy     #0
@d2_y:
        ldx     #0
@d2_x:
        lda     EXP2
        inx
        bne     @d2_x
        iny
        bne     @d2_y

        ; --- read CS3 ---
        ldy     #0
@d3_y:
        ldx     #0
@d3_x:
        lda     CS3
        inx
        bne     @d3_x
        iny
        bne     @d3_y

        dec     anim_ctr
        bne     @pass

        rts

.endproc

; ==============================================================================
; wait.s - cc65 wrappers for 65C02 WAI and STP instructions
; ------------------------------------------------------------------------------

.export _wait, _stop

.code
.proc _wait: near
    cli
    .byte $CB                   ; WAI opcode
    rts
.endproc

.proc _stop: near
    cli
    .byte $DB                   ; STP opcode
    rts
.endproc

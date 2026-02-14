; ==============================================================================
; monitor_core.s - machine-language monitor core
; ------------------------------------------------------------------------------

        .setcpu "65C02"
        .include "mon_const.inc"

; ------------------------------------------------------------------------------
; Exports
; ------------------------------------------------------------------------------

; RODATA tables
        .export hlpmsg, icmd, icmde, iofs
        .export asm_delimiters, asm_mode_data
        .export fscmd, find_mode_flags, find_data_len
        .export reghdr
        .export suffix_mode_bits, suffix_char1, suffix_char2
        .export mode_and_mask, mode_eor_val, mode_inst_len, mode_flags
        .export opcode_mask
        .export opc, impl_opcodes, c02_opcodes, accum_opcodes
        .export opmn1, opmn2, opmn3
        .export asm_mode_test, asm_mode_or, prpow

; CODE entry points
        .export prtint, mon_brk, mon_nmi, _monitor_init
        .export mainloop, error, get_char, ucase, peek_char
        .export print_cr, print_space, print_two_spaces, print_cr_then_x
        .export print_a_then_x
        .export strout, strout1
        .export page_pause, check_stop_key, dispatch, cmdexec
        .export get_opt_addr, get_addr_to_pc, get_three_words, get_start_end
        .export get_two_words, get_word, get_word_alt
        .export get_byte_skip_ws, get_hex_byte, convert_hex_nibble
        .export hex_char_to_val, skip_spaces
        .export print_word, print_hex_byte, print_hex_nibble, inc_addr
        .export check_pause_end, inc_check_end, check_end
        .export store_line_char, erase_eol
        .export decode_opcode, read_operand, branch_target, branch_target_rts
        .export disasm_one, disasm_body, print_instruction, print_mnemonic
        .export disass, disasm_loop, advance_by_len, asm_or_mode
        .export asm_store_bytes, asm_store_loop, asm_bail
        .export asm_set_mode, asm_skip_delim, asm_abs_to_rel, assembler, comma
        .export asm_next_line, asm_one_line, asm_get_line
        .export asm_parse_line, asm_get_mnemonic, asm_store_char
        .export asm_emit_implied, asm_get_operand, asm_finish
        .export memdump, colon, cmd_ascii, cmd_fill, write, wcopy
        .export compute_offset
        .export register, print_binary, semi, go
        .export load, addsub, print_value, print_value_xa, print_value_xy
        .export print_value_sec
        .export cmd_hex, cmd_bin, cmd_dec
        .export tick, equals, find
        .export find_get_pair, find_get_nibble, find_compare
        .export check_relocate, convert_addrs, move, convert, trace
        .export memsiz, memtst
        .export default_irq, twint, tsint, save_brk_vec, restore_brk_vec

; ------------------------------------------------------------------------------
; Imports - zero-page variables (from monitor.s)
; ------------------------------------------------------------------------------
        .importzp mon_termcol, mon_stopflag, mon_lastrecv, mon_rowlimit
        .importzp mon_msgflag, mon_tmpbuf, mon_kbdbuf, mon_kbdcnt
        .importzp mon_lastcol, mon_crflag, mon_lineptr, mon_csrcol
        .importzp mon_csrrow, mon_lastprnt
        .importzp mon_page_cnt, mon_tmp
        .importzp mon_decval
        .importzp mon_work, mon_src_addr, mon_end_addr, mon_find_arg
        .importzp mon_mode, mon_addr_mode, mon_cmd_char, mon_opcode_idx
        .importzp mon_operand
        .importzp mon_scratch1, mon_scratch2, mon_inst_len, mon_asm_col
        .importzp mon_strptr, mon_scrollptr
        .importzp mon_addrs, mon_swap_tmp, mon_addr, mon_end, mon_repeat

; ------------------------------------------------------------------------------
; Imports - BSS variables (from monitor.s)
; ------------------------------------------------------------------------------
        .import mon_irq_vec, mon_brk_vec
        .import mon_membot, mon_memtop
        .import mon_regsave, mon_pchsave, mon_pclsave
        .import mon_srsave, mon_arsave, mon_xrsave, mon_yrsave, mon_spsave
        .import mon_commaflag, mon_irqsave
        .import mon_tracemode, mon_traceaddr, mon_tracecnt
        .import mon_bp1_addr, mon_bp1_save, mon_bp2_addr, mon_bp2_save
        .import mon_bp_count
        .import mon_labels, mon_findhi, mon_findhmsk, mon_findlmsk
        .import mon_linebuf

; ------------------------------------------------------------------------------
; Imports - terminal I/O (from monitor_io.s)
; ------------------------------------------------------------------------------
        .import monio_chrin, monio_chrout, monio_stop, monio_getin
        .import monio_reset
        .import preol, prline, chkcol, proccr

; ------------------------------------------------------------------------------
; Imports - raw I/O (unbuffered, used by load command)
; ------------------------------------------------------------------------------
        .import monio_rawgetc, monio_rawputc

; ------------------------------------------------------------------------------
;                               RODATA
; ------------------------------------------------------------------------------
        .segment "RODATA"

; --- help message -------------------------------------------------------------
hlpmsg:
        .byte   "A xxxx - Assemble starting at x (end assembly with 'f', use Mxx for label)",0
        .byte   "C xxxx yyyy zzzz aaaa bbbb - Convert (execute V followed by W)",0
        .byte   "D xxxx (yyyy) - Disassemble from x (to y)",0
        .byte   "F aa bb cc ..., xxxxx yyyyy - Find byte sequence a b c in x-y",0
        .byte   "FAaaaa, xxxx yyyy - Find absolute address used in opcode",0
        .byte   "FRaaaa, xxxx yyyy - Find relative address used in opcode",0
        .byte   "FTxxxx yyyy - Find table (non-opcode bytes) in x-y",0
        .byte   "FZaa, xxxx yyyy - Find zero-page address used in opcode",0
        .byte   "FIaa, xxxx yyyy - Find immediate argument used in opcode",0
        .byte   "G (xxxx) - Run from x (or current PC)",0
        .byte   "K xxxx (yyyy) - Dump memory from x (to y) as ASCII",0
        .byte   "L - Load Intel HEX data from terminal",0
        .byte   "M xxxx (yyyy) - Dump memory from x (to y) as HEX",0
        .byte   "MS - Check and print memory size",0
        .byte   "MT xxxx yyyy (nn) - Test memory x-y (repeat n times)",0
        .byte   "O xxxx yyyy aa - Fill memory x-y with a",0
        .byte   "TW xxxx - Trace walk (single step)",0
        .byte   "TB xxxx nn - Trace break (set break point at x, stop when hit n times)",0
        .byte   "TQ xxxx - Trace quick (run to break point)",0
        .byte   "TS xxxx - Trace stop (run to x)",0
        .byte   "V xxxx yyyy zzzz aaaa bbbb - Within a-b, convert addresses referencing x-y to z",0
        .byte   "W xxxx yyyy zzzz - Copy memory x-y to z",0
        .byte   "= xxxx yyyy - compare memory starting at x to memory starting at y",0
        .byte   "#ddd - convert DEC to HEX and BIN",0
        .byte   "$xx - convert HEX to DEC and BIN",0
        .byte   "%bbbbbbbb - convert BIN to DEC and HEX",0
        .byte   0

; --- command characters (23 commands) -----------------------------------------
icmd:
        .byte   "'#$%,:;=?ACDFGHKLMORTVWX"
icmde:
        .byte   $00,$00,$00,$00,$00     ; padding

; --- command dispatch table (lo/hi pairs of addr-1 for RTS trick) -------------
iofs:
        .byte   <(tick-1),>(tick-1)     ; '
        .byte   <(cmd_dec-1),>(cmd_dec-1) ; #
        .byte   <(cmd_hex-1),>(cmd_hex-1) ; $
        .byte   <(cmd_bin-1),>(cmd_bin-1) ; %
        .byte   <(comma-1),>(comma-1)   ; ,
        .byte   <(colon-1),>(colon-1)   ; :
        .byte   <(semi-1),>(semi-1)     ; ;
        .byte   <(equals-1),>(equals-1) ; =
        .byte   <(addsub-1),>(addsub-1) ; ?
        .byte   <(assembler-1),>(assembler-1) ; A
        .byte   <(convert-1),>(convert-1) ; C
        .byte   <(disass-1),>(disass-1) ; D
        .byte   <(find-1),>(find-1)     ; F
        .byte   <(go-1),>(go-1)         ; G
        .byte   <(help-1),>(help-1)     ; H
        .byte   <(cmd_ascii-1),>(cmd_ascii-1) ; K
        .byte   <(load-1),>(load-1)     ; L
        .byte   <(memdump-1),>(memdump-1) ; M
        .byte   <(cmd_fill-1),>(cmd_fill-1) ; O
        .byte   <(register-1),>(register-1) ; R
        .byte   <(trace-1),>(trace-1)   ; T
        .byte   <(move-1),>(move-1)     ; V (Variation)
        .byte   <(write-1),>(write-1)   ; W
        .byte   <(exit_monitor-1),>(exit_monitor-1) ; X

; --- line-start characters (auto-detect continuation lines) -------------------
asm_delimiters:
        .byte   "':;,()!"
        .byte   $00,$00,$00

; --- direction increment table (for write) ------------------------------------
asm_mode_data:
        .byte   $FF,$FF,$01,$00

; --- find sub-command tables --------------------------------------------------
fscmd:
        .byte   "AZIRT"                 ; sub-command characters
find_mode_flags:
        .byte   $80,$20,$40,$10,$00     ; mode flags
find_data_len:
        .byte   $02,$01,$01,$02,$00     ; data length (2=word,1=byte,0=none)

; --- register display header --------------------------------------------------
reghdr:
        .byte   $0D,$0D,"  PC  SR AC XR YR SP  NV-BDIZC",$00

; --- disassembler suffix tables -----------------------------------------------
suffix_mode_bits:
        .byte   $02,$04,$01
suffix_char1:
        .byte   $2C,$00,$2C
suffix_char2:
        .byte   $59,$29,$58

; --- addressing mode determination tables (indexed 1..17) ---------------------
mode_and_mask:
        .byte   $9D,$1F,$FF,$1C,$1C,$1F,$1F
        .byte   $1F,$1C,$DF,$1C,$1F,$DF,$FF,$FF
        .byte   $03,$1F

mode_eor_val:
        .byte   $80,$09,$20,$0C,$04,$10,$01
        .byte   $11,$14,$96,$1C,$19,$94,$BE,$6C
        .byte   $03,$13,$01

mode_inst_len:
        .byte   $02,$02,$03,$03,$02,$02,$02
        .byte   $02,$02,$02,$03,$03,$02,$03,$03
        .byte   $03,$02,$00

mode_flags:
        .byte   $40,$40,$80,$80,$20,$10,$25
        .byte   $26,$21,$22,$81,$82,$21,$82,$84
        .byte   $08,$08

opcode_mask:
        .byte   $E7,$E7,$E7,$E7,$E3,$E3,$E3
        .byte   $E3,$E3,$E3,$E3,$E3,$E3,$E3,$E7
        .byte   $A7,$E7,$E7,$F3,$F3,$F7,$DF

; --- opcode tables ------------------------------------------------------------
; opc through accum_opcodes MUST remain contiguous (cross-table indexed access)
opc:
        .byte   $26,$46,$06,$66,$41,$81,$E1
        .byte   $01,$A0,$A2,$A1,$C1,$21,$61,$84
        .byte   $86,$E6,$C6,$E0,$C0,$24,$4C,$20
        .byte   $90,$B0,$F0,$30,$D0,$10,$50,$70
        .byte   $78,$00,$18,$D8,$58,$B8,$CA,$88
        .byte   $E8,$C8,$EA,$48

impl_opcodes:
        .byte   $08,$68,$28,$40,$60,$AA,$A8
        .byte   $BA,$8A,$9A,$98,$38,$F8

c02_opcodes:
        .byte   $89,$9C,$9E,$B2

accum_opcodes:
        .byte   $2A,$4A,$0A,$6A,$4F,$23,$93
        .byte   $B3,$F3,$33,$D3,$13,$53,$73

; --- mnemonic character tables (56 entries each) ------------------------------
opmn1:
        .byte   "RLARESSOLLLCAASSIDCCBJJBBBBBBBBSBCCCCDDIINPPPPRRTTTTTTSS"
opmn2:
        .byte   "OSSOOTBRDDDMNDTTNEPPIMSCCEMNPVVERLLLLEENNOHHLLTTAASXXYEE"
opmn3:
        .byte   "LRLRRACAYXAPDCYXCCXYTPRCSQIELCSIKCDIVXYXYPAPAPISXYXASACD"

; --- assembler helper tables --------------------------------------------------
asm_mode_test:
        .byte   $08,$84,$81,$22,$21,$26,$20,$80
asm_mode_or:
        .byte   $03,$20,$1C,$14,$14,$10,$04,$0C

; --- powers of ten (for decimal output) ---------------------------------------
prpow:
        .word   1, 10, 100, 1000, 10000

; ------------------------------------------------------------------------------
;                                CODE
; ------------------------------------------------------------------------------
        .segment "CODE"

; ------------------------------------------------------------------------------
; prtint - print 16-bit integer in mon_decval as unsigned decimal
; ------------------------------------------------------------------------------
; mon_decval   = high byte
; mon_decval+1 = low byte
; Note: big-endian byte order.
; ------------------------------------------------------------------------------
prtint:
        lda     #0
        ; leading-zero flag (0 = still suppressing)
        sta     mon_opcode_idx
        ldy     #8                      ; offset to highest power (10000)
@prl1:
        ldx     #$FF
        sec                             ; start with digit = -1
@prl2:
        lda     mon_decval+1            ; low byte
        sbc     prpow+0,y
        sta     mon_decval+1
        lda     mon_decval              ; high byte
        sbc     prpow+1,y
        sta     mon_decval
        inx
        bcs     @prl2                   ; loop until underflow
        lda     mon_decval+1            ; add power back in
        adc     prpow+0,y
        sta     mon_decval+1
        lda     mon_decval
        adc     prpow+1,y
        sta     mon_decval
        txa
        ora     mon_opcode_idx          ; combine digit with "seen nonzero" flag
        beq     @prl3                   ; both zero -> leading zero, skip
        sta     mon_opcode_idx          ; mark flag nonzero
        txa                             ; get digit back
        ora     #'0'
        jsr     monio_chrout
@prl3:
        dey
        dey
        bpl     @prl1
        lda     mon_opcode_idx          ; did we print anything?
        bne     @done
        lda     #'0'                    ; value was 0 -> print at least "0"
        jsr     monio_chrout
@done:
        rts

; ------------------------------------------------------------------------------
; _monitor_init - one-time initialization (called from C, __fastcall__)
; ------------------------------------------------------------------------------
_monitor_init:
        jsr     monio_reset
        lda     #<mon_brk
        sta     mon_brk_vec
        lda     #>mon_brk
        sta     mon_brk_vec+1
        lda     #<default_irq
        sta     mon_irq_vec
        lda     #>default_irq
        sta     mon_irq_vec+1
        lda     #0
        ldx     #6                      ; clear 7 bytes of regsave
@clr:
        sta     mon_regsave,x
        dex
        bpl     @clr
        tsx
        stx     mon_spsave
        rts

; ------------------------------------------------------------------------------
; mon_brk - BRK entry point
; ------------------------------------------------------------------------------
; The interrupt handler must push A, X, Y before vectoring here.
; Stack (top->bottom): Y, X, A, SR, PCL, PCH  (6 bytes)
; ------------------------------------------------------------------------------
mon_brk:
        cld
        ldx     #$05
@save:
        pla
        sta     mon_regsave,x           ; save Y,X,A,SR,PCL,PCH
        dex
        bpl     @save
        lda     mon_pclsave
        bne     @nolo
        dec     mon_pchsave             ; adjust PC (BRK pushes PC+2)
@nolo:
        dec     mon_pclsave
        tsx
        stx     mon_spsave
        lda     #'R'                    ; show registers on BRK entry
        jmp     cmdexec

; ------------------------------------------------------------------------------
; mon_nmi - NMI entry point
; ------------------------------------------------------------------------------
; NMI hardware pushes PCH, PCL, SR.  This handler pushes A, X, Y and
; saves all six bytes into mon_regsave, then enters the monitor.
; Unlike mon_brk, no PC adjustment is needed.
; ------------------------------------------------------------------------------
mon_nmi:
        cld
        ldx     #$05
@save:
        pla
        sta     mon_regsave,x           ; save Y,X,A,SR,PCL,PCH
        dex
        bpl     @save
        tsx
        stx     mon_spsave
        lda     #'R'                    ; show registers on NMI entry
        jmp     cmdexec

; ------------------------------------------------------------------------------
; mainloop - main command loop
; ------------------------------------------------------------------------------
mainloop:
        ldx     mon_spsave
        txs                             ; reset stack
        ldx     #$00
        stx     mon_kbdcnt              ; clear keyboard buffer
        lda     #NUMROWS-2
        sta     mon_page_cnt            ; reset page line counter
        lda     mon_csrcol
        beq     @skipcr
        jsr     print_cr                ; print CR if not at col 0
@skipcr:
        lda     (mon_lineptr,x)         ; first char of current line (X=0)
        ldx     #$06
@chkln:
        cmp     asm_delimiters,x        ; known line-start character?
        beq     @noprompt
        dex
        bpl     @chkln
        lda     #'.'                    ; print prompt
        jsr     monio_chrout
@noprompt:
        jsr     get_char                ; read command character
        cmp     #'.'
        beq     @noprompt               ; ignore leading dots
        ; fall through to cmdexec

; ------------------------------------------------------------------------------
; cmdexec - look up command in A and execute it
; ------------------------------------------------------------------------------
cmdexec:
        sta     mon_cmd_char
        and     #$7F
        ldx     #icmde-icmd
fndcmd:
        cmp     icmd-1,x                ; compare command char
        beq     @found
        dex
        bne     fndcmd
        jmp     error                   ; no match
@found:
        jsr     dispatch                ; execute command
        jmp     mainloop                ; back to main loop

; ------------------------------------------------------------------------------
; dispatch - execute command by index in X
; ------------------------------------------------------------------------------
; Uses RTS trick: push (handler_addr - 1), then RTS jumps to handler.
; iofs stores interleaved lo,hi pairs of (addr-1).
; ------------------------------------------------------------------------------
dispatch:
        txa
        asl
        tax
        inx
        lda     iofs-2,x                ; high byte of (handler-1)
        pha
        dex
        lda     iofs-2,x                ; low byte of (handler-1)
        pha
        rts                             ; -> handler

; ------------------------------------------------------------------------------
; error - print "?" and return to main loop
; ------------------------------------------------------------------------------
error:
        lda     #'?'
        jsr     monio_chrout
        jmp     mainloop

; ------------------------------------------------------------------------------
; get_char - get next command-line character
; ------------------------------------------------------------------------------
; Returns uppercase character in A.  If CR, jumps to error.
; ------------------------------------------------------------------------------
get_char:
        jsr     monio_chrin
        jsr     ucase
        cmp     #$0D
        bne     @ok
        jmp     error                   ; unexpected end of input
@ok:
        rts

; ------------------------------------------------------------------------------
; ucase - convert A to uppercase
; ------------------------------------------------------------------------------
ucase:
        cmp     #'a'
        bcc     @done
        cmp     #'z'+1
        bcs     @done
        and     #$DF
@done:
        rts

; ------------------------------------------------------------------------------
; peek_char - peek at next character, Z set if CR
; ------------------------------------------------------------------------------
; Reads one character via monio_chrin then backs up the cursor column
; so the character can be re-read.  Z flag set if CR.
; ------------------------------------------------------------------------------
peek_char:
        jsr     monio_chrin
        dec     mon_csrcol
        cmp     #$0D
        rts

; ------------------------------------------------------------------------------
; Output helpers
; ------------------------------------------------------------------------------

; --- print_cr: output CR ------------------------------------------------------
print_cr:
        lda     #$0D
        jmp     monio_chrout

; --- print_space: output SPACE ------------------------------------------------
print_space:
        lda     #$20
        jmp     monio_chrout

; --- print_two_spaces: output two SPACEs --------------------------------------
print_two_spaces:
        jsr     print_space
        jmp     print_space

; --- print_cr_then_x: output CR then character in X ---------------------------
print_cr_then_x:
        lda     #$0D
; --- print_a_then_x: output A then X as characters ----------------------------
print_a_then_x:
        jsr     monio_chrout
        txa
        jmp     monio_chrout

; ------------------------------------------------------------------------------
; strout - print 0-terminated string, pointer in A(lo)/Y(hi)
; ------------------------------------------------------------------------------
strout:
        sta     mon_strptr
        sty     mon_strptr+1
        ldy     #$00
strout1:
        lda     (mon_strptr),y
        beq     @done
        jsr     monio_chrout
        inc     mon_strptr
        bne     strout1
        inc     mon_strptr+1
        bne     strout1                 ; always taken
@done:
        rts

; ------------------------------------------------------------------------------
; help - H command
; ------------------------------------------------------------------------------
help:
        lda     #<hlpmsg
        sta     mon_strptr
        lda     #>hlpmsg
        sta     mon_strptr+1
        ldy     #$00
@line:
        lda     #$0D
        jsr     monio_chrout
        jsr     strout1                 ; print string until 0 terminator
        iny                             ; advance past 0 to next string
        cpy     #20
        bne     @nopause
        lda     #' '                    ; push SPACE to keyboard buffer
        sta     mon_kbdbuf              ; (pauses output after next line)
        inc     mon_kbdcnt
@nopause:
        jsr     page_pause              ; check PAUSE/STOP
        lda     (mon_strptr),y          ; first byte of next string
        bne     @line                   ; loop if not end sentinel
        rts

; ------------------------------------------------------------------------------
; page_pause - end-of-line wait/pause handling
; ------------------------------------------------------------------------------
; If a key was pressed, wait for another keypress to resume.
; If SPACE, push it to kbdbuf so the next line pauses too.
; If STOP, longjmp to mainloop.
; ------------------------------------------------------------------------------
page_pause:
        dec     mon_page_cnt            ; decrement page line counter
        bmi     @autopause              ; page full -> force pause
        jsr     check_stop_key          ; poll for keypress
        beq     @done                   ; no key -> done
        bne     @dowait                 ; key pressed -> pause
@autopause:
        lda     #NUMROWS-2
        sta     mon_page_cnt            ; reset counter
@dowait:
        jsr     check_stop_key
        beq     @dowait                 ; wait for resume keypress
        cmp     #$20                    ; SPACE?
        bne     @done
        sta     mon_kbdbuf
        inc     mon_kbdcnt              ; single-step mode
@done:
        rts

; ------------------------------------------------------------------------------
; check_stop_key - check for STOP key or keypress
; ------------------------------------------------------------------------------
; Returns: A = char (0 if none), Z set if no key.
; If STOP pressed, longjmps to mainloop.
; ------------------------------------------------------------------------------
check_stop_key:
        jsr     monio_getin
        pha
        jsr     monio_stop
        beq     @stopped
        pla                             ; restore char to A
        rts
@stopped:
        ; STOP pressed -> abort (stack reset there)
        jmp     mainloop

; ------------------------------------------------------------------------------
; get_opt_addr - optionally get address for G command
; ------------------------------------------------------------------------------
; If next char is not CR, read a word into PCLSAVE/PCHSAVE.
; ------------------------------------------------------------------------------
get_opt_addr:
        jsr     peek_char
        beq     get_opt_addr_rts
get_addr_to_pc:
        jsr     get_word
        sta     mon_pclsave
        lda     mon_addr+1
        sta     mon_pchsave
get_opt_addr_rts:
        rts

; ------------------------------------------------------------------------------
; get_three_words - get 3 words into mon_src_addr-mon_find_arg+1
; ------------------------------------------------------------------------------
get_three_words:
        ldx     #mon_src_addr
        jsr     get_word_alt
        jsr     get_word_alt
        ; always taken (X != 0 after get_word_alt)
        bne     get_word_alt

; ------------------------------------------------------------------------------
; get_start_end - get start/end address from command line
; ------------------------------------------------------------------------------
; Start address -> mon_addr/mon_addr+1
; End address   -> mon_end/mon_end+1 (defaults to $FFFE if omitted)
; ------------------------------------------------------------------------------
get_start_end:
        jsr     get_word
        lda     #$FE
        sta     mon_end
        lda     #$FF
        sta     mon_end+1
        jsr     peek_char               ; more input?
        bne     get_word_alt            ; yes -> get end address
        lda     #$00
        sta     mon_kbdbuf              ; put NUL into keyboard buffer
        inc     mon_kbdcnt
        rts

; ------------------------------------------------------------------------------
; get_two_words - get two words into mon_addr and mon_end
; ------------------------------------------------------------------------------
get_two_words:
        jsr     get_word
        .byte   $2C                     ; BIT abs - skip next 2-byte opcode

; ------------------------------------------------------------------------------
; get_word - get word from command line into mon_addr/mon_addr+1
; ------------------------------------------------------------------------------
get_word:
        ldx     #mon_addr

; ------------------------------------------------------------------------------
; get_word_alt - get word from command line, store at ZP (X)/(X+1)
; ------------------------------------------------------------------------------
; X holds zero-page address of destination.
; Uses sta $00,x / sta $01,x (ZP,X addressing wraps within page 0).
; ------------------------------------------------------------------------------
get_word_alt:
        ; get high byte (skip leading spaces/commas)
        jsr     get_byte_skip_ws
        sta     $01,x                   ; store high byte at (X+1)
        jsr     get_hex_byte            ; get low byte
        sta     $00,x                   ; store low byte at (X)
        inx
        inx
        rts

; ------------------------------------------------------------------------------
; get_byte_skip_ws - get byte from command line, skip leading spaces/commas
; ------------------------------------------------------------------------------
get_byte_skip_ws:
        jsr     get_char
        cmp     #$20
        beq     get_byte_skip_ws
        cmp     #$2C
        beq     get_byte_skip_ws
        bne     convert_hex_nibble      ; always taken

; ------------------------------------------------------------------------------
; get_hex_byte - get hex byte from command line
; ------------------------------------------------------------------------------
get_hex_byte:
        jsr     get_char
convert_hex_nibble:
        jsr     hex_char_to_val         ; convert high nibble
        asl
        asl
        asl
        asl
        sta     mon_scratch1
        jsr     get_char
        jsr     hex_char_to_val         ; convert low nibble
        ora     mon_scratch1
        rts

; ------------------------------------------------------------------------------
; hex_char_to_val - convert ASCII hex character to 0-15
; ------------------------------------------------------------------------------
hex_char_to_val:
        cmp     #$3A                    ; > '9'?
        bcc     @digit
        adc     #$08                    ; adjust for A-F
@digit:
        and     #$0F
        rts

; ------------------------------------------------------------------------------
; skip_spaces - skip spaces from command line
; ------------------------------------------------------------------------------
skip_spaces:
        jsr     get_char
        cmp     #$20
        beq     skip_spaces
        dec     mon_csrcol
        rts

; ------------------------------------------------------------------------------
; print_word - output word in mon_addr+1/mon_addr as 4-digit hex
; ------------------------------------------------------------------------------
print_word:
        lda     mon_addr+1
        jsr     print_hex_byte
        lda     mon_addr
        ; fall through to print_hex_byte

; ------------------------------------------------------------------------------
; print_hex_byte - output byte in A as 2-digit hex
; ------------------------------------------------------------------------------
print_hex_byte:
        pha
        lsr
        lsr
        lsr
        lsr
        jsr     print_hex_nibble
        pla
        and     #$0F
; --- print_hex_nibble: output nibble 0-15 as hex character --------------------
print_hex_nibble:
        cmp     #$0A
        bcc     @digit
        adc     #$06                    ; 'A'-'0'-10 + carry
@digit:
        adc     #$30                    ; '0'
        jmp     monio_chrout

; ------------------------------------------------------------------------------
; inc_addr - increment address in mon_addr/mon_addr+1
; ------------------------------------------------------------------------------
inc_addr:
        inc     mon_addr
        bne     @done
        inc     mon_addr+1
@done:
        rts

; ------------------------------------------------------------------------------
; check_pause_end - check stop/pause + end address
; ------------------------------------------------------------------------------
check_pause_end:
        jsr     page_pause              ; end-of-line wait handling
        jmp     check_end               ; check end address

; ------------------------------------------------------------------------------
; inc_check_end - increment mon_addr and check end address
; ------------------------------------------------------------------------------
inc_check_end:
        jsr     inc_addr

; ------------------------------------------------------------------------------
; check_end - check whether end address reached
; ------------------------------------------------------------------------------
; C clear = not reached; C set = reached.
; ------------------------------------------------------------------------------
check_end:
        lda     mon_addr
        cmp     mon_end
        lda     mon_addr+1
        sbc     mon_end+1
        rts

; ------------------------------------------------------------------------------
; store_line_char - put character into line buffer at column Y
; ------------------------------------------------------------------------------
; Ensures the character is printable (replaces non-printable with '.').
; Increments mon_addr and Y.  Returns with Z set if Y == NUMCOLS.
; ------------------------------------------------------------------------------
store_line_char:
        cmp     #$20
        bcc     @dot                    ; < space -> dot
        cmp     #$7F
        bcs     @dot                    ; >= DEL -> dot
        bcc     @store                  ; printable -> store as-is
@dot:
        lda     #$2E                    ; '.'
@store:
        sta     (mon_lineptr),y
        jsr     inc_addr                ; increment mon_addr
        iny
        cpy     #NUMCOLS
        rts

; ------------------------------------------------------------------------------
; erase_eol - erase screen buffer from cursor column to end of line
; ------------------------------------------------------------------------------
; Erases from cursor column to NUMCOLS using (mon_lineptr),y.
; ------------------------------------------------------------------------------
erase_eol:
        ldy     mon_csrcol
        lda     #' '
@loop:
        sta     (mon_lineptr),y
        iny
        cpy     #NUMCOLS
        bcc     @loop
        rts

; ------------------------------------------------------------------------------
; decode_opcode - decode opcode at (mon_addr), set addressing mode and length
; ------------------------------------------------------------------------------
; Returns: mon_opcode_idx = opcode index (0 = unknown), mon_addr_mode =
; addressing mode flags,
;          mon_inst_len = instruction length.  X = opcode index.
; ------------------------------------------------------------------------------
decode_opcode:
        ldy     #$00
        lda     (mon_addr),y            ; get opcode
        bit     mon_mode
        bmi     @chk1
        bvc     @chk2
@chk1:
        ldx     #$1F
@lp1:
        cmp     impl_opcodes-1,x
        beq     @found
        dex
        cpx     #$15
        bne     @lp1
@chk2:
        ldx     #$04
@lp2:
        cmp     c02_opcodes-1,x
        beq     @noidx
        cmp     accum_opcodes-1,x
        beq     @found
        dex
        bne     @lp2
        ldx     #$38
@lp3:
        cmp     opc-1,x
        beq     @found
        dex
        cpx     #$16
        bne     @lp3
@lp4:
        lda     (mon_addr),y
        and     opcode_mask-1,x
        eor     opc-1,x
        beq     @found
        dex
        bne     @lp4
@noidx:
        ldx     #$00
@found:
        stx     mon_opcode_idx
        txa
        beq     @setmode
        ldx     #$11
@lp5:
        lda     (mon_addr),y
        and     mode_and_mask-1,x
        eor     mode_eor_val-1,x
        beq     @setmode
        dex
        bne     @lp5
@setmode:
        lda     mode_flags-1,x
        sta     mon_addr_mode
        lda     mode_inst_len-1,x
        sta     mon_inst_len
        ldx     mon_opcode_idx
        rts

; ------------------------------------------------------------------------------
; read_operand - read operand bytes from (mon_addr)+1/+2 into
; mon_operand/mon_operand+1
; ------------------------------------------------------------------------------
read_operand:
        ldy     #$01
        lda     (mon_addr),y
        tax
        iny
        lda     (mon_addr),y
        ldy     #$10
        cpy     mon_addr_mode
        bne     @notrel
        jsr     branch_target
        ldy     #$03
        bne     @store
@notrel:
        ldy     mon_inst_len
@store:
        stx     mon_operand
        nop
        sta     mon_operand+1
        nop
        rts

; ------------------------------------------------------------------------------
; branch_target - compute relative branch target address
; ------------------------------------------------------------------------------
branch_target:
        ldy     #$01
        lda     (mon_addr),y
        bpl     @pos
        dey
@pos:
        sec
        adc     mon_addr
        tax
        inx
        beq     @wrap
        dey
@wrap:
        tya
        adc     mon_addr+1
branch_target_rts:
        rts

; ------------------------------------------------------------------------------
; disass - D command
; ------------------------------------------------------------------------------
disass:
        ldx     #$00
        stx     mon_mode
        jsr     get_start_end
disasm_loop:
        jsr     disasm_one              ; disassemble one line
        lda     mon_opcode_idx
        cmp     #$16                    ; JMP abs?
        beq     @sep
        cmp     #$30                    ; RTS?
        beq     @sep
        cmp     #$21                    ; JSR abs?
        bne     @nosep
        nop
@sep:
        jsr     print_cr                ; print separator line
        ldx     #$23
        lda     #$2D
@dash:
        jsr     monio_chrout
        dex
        bne     @dash
@nosep:
        jsr     check_pause_end         ; check pause/stop/end
        bcc     disasm_loop
        rts

; ------------------------------------------------------------------------------
; disasm_one - disassemble one instruction at mon_addr
; ------------------------------------------------------------------------------
disasm_one:
        ldx     #$2C                    ; ','
        jsr     print_cr_then_x         ; CR then ','
        jsr     print_word              ; print address
        jsr     print_space             ; space
disasm_body:
        jsr     erase_eol               ; erase to end of line
        jsr     decode_opcode           ; decode opcode
        jsr     print_space             ; space
        ; print raw bytes
        ldy     #$00
@raw:
        lda     (mon_addr),y
        jsr     print_hex_byte          ; hex byte
        jsr     print_space             ; space
        iny
        cpy     mon_inst_len
        bne     @raw
        ; pad to 3 bytes width
        lda     #$03
        sec
        sbc     mon_inst_len
        tax
        beq     @padded
@pad:
        jsr     print_two_spaces        ; two spaces
        jsr     print_space             ; one more space
        dex
        bne     @pad
@padded:
        lda     #$20
        jsr     monio_chrout
        ldy     #$00
        ldx     mon_opcode_idx
print_instruction:
        bne     print_mnemonic
        ; unknown opcode - print "***"
        ldx     #$03
@star:
        lda     #$2A
        jsr     monio_chrout
        dex
        bne     @star
        bit     mon_mode
        bmi     branch_target_rts       ; assembler mode: just return
        jmp     advance_by_len          ; advance FB by instruction length
print_mnemonic:
        bit     mon_mode
        bvc     @nolabel
        ; check for label reference (assembler mode with labels)
        lda     #$08
        bit     mon_addr_mode
        beq     @nolabel
        lda     (mon_addr),y
        and     #$FC
        sta     mon_opcode_idx
        iny
        lda     (mon_addr),y
        asl
        tay
        lda     mon_labels,y
        sta     mon_operand
        nop
        iny
        lda     mon_labels,y
        sta     mon_operand+1
        nop
        jsr     asm_abs_to_rel
        ldy     mon_inst_len
        jsr     asm_store_loop
        jsr     decode_opcode
@nolabel:
        ; print mnemonic
        lda     opmn1-1,x
        jsr     monio_chrout
        lda     opmn2-1,x
        jsr     monio_chrout
        lda     opmn3-1,x
        jsr     monio_chrout
        lda     #$20
        bit     mon_addr_mode
        beq     @twospc
        jsr     print_two_spaces        ; two spaces
@twospc:
        ldx     #$20                    ; default: space before operand
        lda     #$04
        bit     mon_addr_mode
        beq     @noparen
        ldx     #$28                    ; '(' if indirect
@noparen:
        txa
        jsr     monio_chrout
        bit     mon_addr_mode
        bvc     @noimm
        lda     #$23                    ; '#'
        jsr     monio_chrout
@noimm:
        jsr     read_operand            ; read operand
        dey
        beq     @nooperand
        lda     #$08
        bit     mon_addr_mode
        beq     @nolbl2
        lda     #$4D                    ; 'M'
        jsr     monio_chrout
        ldy     #$01
@nolbl2:
        lda     mon_opcode_idx,y
        jsr     print_hex_byte          ; print operand byte(s)
        dey
        bne     @nolbl2
@nooperand:
        ldy     #$03
@suffix:
        lda     suffix_mode_bits-1,y
        bit     mon_addr_mode
        beq     @skipsuf
        lda     suffix_char1-1,y
        ldx     suffix_char2-1,y
        jsr     print_a_then_x          ; print suffix char pair
@skipsuf:
        dey
        bne     @suffix

; ------------------------------------------------------------------------------
; advance_by_len - advance mon_addr by instruction length (mon_inst_len)
; ------------------------------------------------------------------------------
advance_by_len:
        lda     mon_inst_len
@adv:
        jsr     inc_addr
        sec
        sbc     #$01
        bne     @adv
        rts

; ------------------------------------------------------------------------------
; asm_or_mode - OR value into mon_opcode_idx if addressing mode matches
; (assembler helper)
; ------------------------------------------------------------------------------
asm_or_mode:
        cpx     mon_addr_mode
        bne     @skip
        ora     mon_opcode_idx
        sta     mon_opcode_idx
@skip:
        rts

; ------------------------------------------------------------------------------
; asm_store_bytes - copy mon_opcode_idx..mon_opcode_idx+Y to (mon_addr), verify
; writes
; ------------------------------------------------------------------------------
asm_store_bytes:
        lda     mon_opcode_idx,y
        sta     (mon_addr),y
        cmp     (mon_addr),y
        bne     asm_bail
asm_store_loop:
        dey
        bpl     asm_store_bytes
        rts

asm_bail:
        pla                             ; discard return address (bail out)
        pla
        rts

; ------------------------------------------------------------------------------
; asm_set_mode - set addressing mode flag if delimiter matches (assembler
; helper)
; ------------------------------------------------------------------------------
asm_set_mode:
        bne     @skip
        txa
        ora     mon_addr_mode
        sta     mon_addr_mode
@skip:
        rts                             ; (LC6B8)

; ------------------------------------------------------------------------------
; asm_skip_delim - get first non-delimiter character from input (assembler
; helper)
; ------------------------------------------------------------------------------
; Skips up to 4 occurrences of ' ', '$', '(', ','
; ------------------------------------------------------------------------------
asm_skip_delim:
        lda     #$04
        sta     mon_scratch2
@loop:
        jsr     monio_chrin
        cmp     #$20                    ; space
        beq     @delim
        cmp     #$24                    ; '$'
        beq     @delim
        cmp     #$28                    ; '('
        beq     @delim
        cmp     #$2C                    ; ','
        beq     @delim
        jsr     ucase
        rts
@delim:
        dec     mon_scratch2
        bne     @loop
        rts

; ------------------------------------------------------------------------------
; asm_abs_to_rel - convert absolute address to relative for branch (assembler
; helper)
; ------------------------------------------------------------------------------
asm_abs_to_rel:
        cpx     #$18
        bmi     @done
        lda     mon_operand
        nop
        sec
        sbc     #$02
        sec
        sbc     mon_addr
        sta     mon_operand
        nop
        ldy     #$40
@done:
        rts

; ------------------------------------------------------------------------------
; assembler - A command
; ------------------------------------------------------------------------------
assembler:
        jsr     get_word                ; get start address
        sta     mon_end
        lda     mon_addr+1
        sta     mon_end+1
asm_next_line:
        jsr     print_cr                ; print CR
asm_one_line:
        jsr     asm_get_line            ; get and assemble one line
        bmi     asm_one_line
        bpl     asm_next_line

; ------------------------------------------------------------------------------
; asm_get_line - assemble one line
; ------------------------------------------------------------------------------
asm_get_line:
        lda     #$00
        sta     mon_csrcol
        jsr     print_space             ; output space
        jsr     print_word              ; output address
        jsr     print_space             ; output space
        jsr     monio_chrin              ; get character (prime input)
        lda     #$01
        sta     mon_csrcol
        ldx     #$80
        bne     asm_parse_line

; ------------------------------------------------------------------------------
; comma - , command, assemble single line
; ------------------------------------------------------------------------------
comma:
        ldx     #$80
        stx     mon_commaflag
asm_parse_line:
        stx     mon_mode
        jsr     get_word                ; get address
        lda     #$25                    ; set last input column
        sta     mon_lastcol
        bit     mon_commaflag
        bpl     @nocomma
        ldx     #$0A                    ; skip 10 chars (for ',' command)
@skip:
        jsr     monio_chrin
        dex
        bne     @skip
@nocomma:
        lda     #$00
        sta     mon_commaflag
        jsr     asm_skip_delim          ; get character (skip delimiters)
        cmp     #$46                    ; 'F' - finish assembly?
        bne     @notf
        ; exit assembler: disassemble the result
        lsr     mon_mode
        pla
        pla
        ldx     #$02
; swap mon_addr/mon_addr+1 and mon_end/mon_end+1
@swap:
        lda     mon_swap_tmp,x
        pha
        lda     mon_addr+1,x
        sta     mon_swap_tmp,x
        pla
        sta     mon_addr+1,x
        dex
        bne     @swap
        jmp     disasm_loop             ; disassemble
@notf:
        cmp     #$2E                    ; '.' - raw byte?
        bne     @notdot
        jsr     get_hex_byte
        ldy     #$00
        sta     (mon_addr),y
        cmp     (mon_addr),y
        bne     @dotfail
        jsr     inc_addr
        iny
@dotfail:
        dey
        rts

@notdot:
        ldx     #$FD
        cmp     #$4D                    ; 'M' - label definition?
        bne     asm_store_char
        jsr     get_hex_byte
        ldy     #$00
        cmp     #$3F                    ; max 63 labels
        bcs     @dotfail
        asl
        tay
        lda     mon_addr
        sta     mon_labels,y
        lda     mon_addr+1
        iny
        sta     mon_labels,y
        ; read 3 opcode characters
asm_get_mnemonic:
        jsr     asm_skip_delim
asm_store_char:
        ; store into mon_end_addr/mon_end_addr+1/mon_find_arg (X starts at $FD)
        sta     mon_find_arg+1,x
        cpx     #$FD
        bne     @notfirst
        lda     #$07
        sta     mon_asm_col
@notfirst:
        inx
        bne     asm_get_mnemonic        ; get 3 characters total
        ; find mnemonic in table
        ldx     #$38
@findmn:
        lda     mon_end_addr
        cmp     opmn1-1,x
        beq     @chk2nd
@nextmn:
        dex
        bne     @findmn
        dex                             ; X=$FF -> N flag set -> error exit
        rts
@chk2nd:
        lda     mon_end_addr+1
        cmp     opmn2-1,x
        bne     @nextmn
        lda     mon_find_arg
        cmp     opmn3-1,x
        bne     @nextmn
        ; found mnemonic
        lda     opc-1,x
        sta     mon_opcode_idx
        jsr     asm_skip_delim          ; get operand start character
        ldy     #$00
        cpx     #$20
        bpl     asm_emit_implied        ; implied/accumulator: no operand
        cmp     #$20                    ; space -> accumulator variant
        bne     asm_hasop
        lda     accum_opcodes-1,x
        sta     mon_opcode_idx
asm_emit_implied:
        jmp     asm_finish

asm_hasop:
        ldy     #$08
        cmp     #$4D                    ; 'M' - label reference?
        beq     asm_get_operand
        ldy     #$40
        cmp     #$23                    ; '#' - immediate?
        beq     asm_get_operand
        jsr     convert_hex_nibble      ; convert first hex nibble
        sta     mon_operand
        nop
        sta     mon_operand+1
        nop
        jsr     asm_skip_delim
        ldy     #$20
        cmp     #$30                    ; < '0'?
        bcc     asm_oneop
        cmp     #$47                    ; >= 'G'?
        bcs     asm_oneop
        ldy     #$80
        dec     mon_csrcol
asm_get_operand:
        jsr     asm_skip_delim
        jsr     convert_hex_nibble      ; convert operand
        sta     mon_operand
        nop
        jsr     asm_skip_delim
        cpy     #$08
        beq     asm_oneop
        jsr     asm_abs_to_rel          ; convert to relative if branch
asm_oneop:
        sty     mon_addr_mode
        ldx     #$01
        cmp     #$58                    ; 'X'?
        jsr     asm_set_mode
        ldx     #$04
        cmp     #$29                    ; ')'?
        jsr     asm_set_mode
        ldx     #$02
        cmp     #$59                    ; 'Y'?
        jsr     asm_set_mode
        lda     mon_opcode_idx
        and     #$0D
        beq     @nozp
        ldx     #$40
        lda     #$08
        jsr     asm_or_mode
        lda     #$18
        .byte   $2C                     ; skip next 2-byte opcode
@nozp:
        lda     #$1C
        ldx     #$82
        jsr     asm_or_mode
        ldy     #$08
        lda     mon_opcode_idx
        cmp     #$20                    ; JSR?
        beq     @donemode
@modeloop:
        ldx     asm_mode_test-1,y
        lda     asm_mode_or-1,y
        jsr     asm_or_mode
@donemode:
        dey
        bne     @modeloop
        lda     mon_addr_mode
        bpl     @onebyte
        iny                             ; Y=1 -> 2-byte operand
@onebyte:
        iny                             ; Y=1 or Y=2
asm_finish:
        jsr     asm_store_bytes         ; copy opcode + operands to memory
        dec     mon_asm_col
        lda     mon_asm_col
        sta     mon_csrcol
        jmp     disasm_body             ; disassemble the result

; ------------------------------------------------------------------------------
; memdump - M command
; ------------------------------------------------------------------------------
; M xxxx (yyyy) - hex dump
; MS            - memory size (stub)
; MT xxxx yyyy  - memory test (stub)
; ------------------------------------------------------------------------------
memdump:
        jsr     get_char
        ; error if CR (defensive, get_char never returns on CR)
        beq     mderr
        cmp     #'T'
        bne     md1
        jmp     memtst
md1:
        cmp     #'S'
        bne     md2
        jmp     memsiz
md2:
        cmp     #' '
        bne     mderr
        jsr     get_start_end           ; get start (mon_addr) and end (mon_end)
md_hex_line:
        ldx     #$3A                    ; ':'
        jsr     print_cr_then_x         ; print CR then ':'
        jsr     print_word              ; print address in mon_addr
        ldy     #NUMCOLS-17
        ldx     #0
md_print_byte:
        jsr     print_space             ; print space
        cpy     #NUMCOLS-9
        bne     md_ascii_byte
        jsr     print_space             ; extra space (visual gap)
        iny
md_ascii_byte:
        lda     (mon_addr,x)
        jsr     print_hex_byte          ; print byte as hex
        lda     (mon_addr,x)
        jsr     store_line_char         ; store ASCII in line buffer
        bne     md_print_byte           ; repeat until Y == NUMCOLS
        jsr     preol                   ; print rest of line
        jsr     check_pause_end         ; check pause/stop/end
        bcc     md_hex_line             ; continue if not at end
        rts

mderr:
        jmp     error

; ------------------------------------------------------------------------------
; memsiz - MS sub-command, scan for RAM boundaries
; ------------------------------------------------------------------------------
memsiz:
        ldx     #$01
        stx     mon_addr+1              ; start at $0100
        dex
        stx     mon_addr                ; low = $00
        dex
        stx     mon_end                 ; end low = $FF
        stx     mon_end+1               ; end high = $FF
        jsr     print_cr
        ldx     #$00
ms_test:
        lda     (mon_addr,x)            ; save current value
        tay
        lda     #$55
        sta     (mon_addr,x)
        cmp     (mon_addr,x)
        bne     ms_fail
        lda     #$AA
        sta     (mon_addr,x)
        cmp     (mon_addr,x)
        bne     ms_fail
        tya
        sta     (mon_addr,x)            ; restore original
        jsr     inc_addr                ; increment mon_addr
        jsr     check_end               ; check end
        bcc     ms_test
        .byte   $2C                     ; BIT abs - skip next instruction
ms_fail:
        tya
        sta     (mon_addr,x)            ; restore on failure too
        jsr     print_word              ; print address where RAM ends
        rts

; ------------------------------------------------------------------------------
; memtst - MT sub-command, test RAM range with patterns
; ------------------------------------------------------------------------------
memtst:
        ldx     #mon_src_addr
        jsr     get_word_alt            ; start address
        jsr     get_word_alt            ; end address
        ldy     #1                      ; default 1 repetition
        jsr     peek_char
        beq     mt_go
        jsr     get_byte_skip_ws
        tay
mt_go:
        sty     mon_repeat
        lda     mon_src_addr+1
        bne     mt_ok
        jmp     error                   ; can't test page 0
mt_ok:
        jsr     print_cr
mt_rep:
        ldx     #3
mt_cp:
        lda     mon_src_addr,x          ; copy a4-a7 -> fb-fd+1
        sta     mon_addr,x
        dex
        bpl     mt_cp
        ldx     #$00
        lda     #$00                    ; pattern 0: $00
        jsr     mt_pat
        lda     #$55                    ; pattern 1: $55
        jsr     mt_pat
        lda     #$AA                    ; pattern 2: $AA
        jsr     mt_pat
        lda     #$FF                    ; pattern 3: $FF
        jsr     mt_pat
        dec     mon_repeat
        bne     mt_rep
        rts
; test pattern in A across mon_addr..mon_end
mt_pat:
        sta     mon_scratch1            ; save pattern
        ; reset pointers
        ldx     #3
mt_cp2:
        lda     mon_src_addr,x
        sta     mon_addr,x
        dex
        bpl     mt_cp2
        ldx     #$00
        ; write pattern
mt_wr:
        lda     mon_scratch1
        sta     (mon_addr,x)
        jsr     inc_addr
        jsr     check_end
        bcc     mt_wr
        ; reset pointers
        ldx     #3
mt_cp3:
        lda     mon_src_addr,x
        sta     mon_addr,x
        dex
        bpl     mt_cp3
        ldx     #$00
        ; verify pattern
mt_vfy:
        lda     (mon_addr,x)
        cmp     mon_scratch1
        bne     mt_err
        jsr     inc_addr
        jsr     check_end
        bcc     mt_vfy
        lda     #'+'
        jsr     monio_chrout
        rts
mt_err:
        jsr     print_word              ; print failing address
        jmp     error

; ------------------------------------------------------------------------------
; colon - : command, edit memory dump
; ------------------------------------------------------------------------------
colon:
        jsr     get_word                ; get start address -> mon_addr
        ldy     #NUMCOLS-17
        ldx     #$00
col_check_width:
        cpy     #NUMCOLS-9
        bne     col_get_char
        iny                             ; skip gap column
col_get_char:
        jsr     monio_chrin              ; get next input character
        cmp     #$20                    ; space?
        beq     col_check_width         ; skip
        cmp     #$0D                    ; CR?
        beq     colod                   ; done
        dec     mon_csrcol              ; back up cursor (re-read char)
        jsr     get_hex_byte            ; parse hex byte
        sta     (mon_addr,x)            ; store byte at address
        cmp     (mon_addr,x)            ; verify (detects ROM)
        beq     col_inc_addr
        jmp     error                   ; write verify failed
col_inc_addr:
        jsr     inc_addr                ; increment mon_addr
        iny
        cpy     #NUMCOLS
        bne     col_check_width         ; repeat until Y == NUMCOLS
colod:
        rts

; ------------------------------------------------------------------------------
; cmd_ascii - K command, ASCII dump
; ------------------------------------------------------------------------------
cmd_ascii:
        jsr     get_start_end           ; get start/end address
kloop:
        ldx     #$27                    ; "'"
        jsr     print_cr_then_x         ; print CR then "'"
        jsr     print_word              ; print address
        ldy     #$08                    ; ASCII starts at column 8
        ldx     #$00
        jsr     print_space             ; print space
kchr:
        lda     (mon_addr,x)            ; get byte
        jsr     store_line_char         ; store ASCII in line buffer
        bne     kchr                    ; repeat until Y == NUMCOLS
        jsr     preol                   ; print rest of line
        ldx     #$00
        jsr     check_pause_end         ; check pause/stop/end
        bcc     kloop                   ; continue if not at end
        rts

; ------------------------------------------------------------------------------
; cmd_fill - O command, fill memory
; ------------------------------------------------------------------------------
cmd_fill:
        jsr     get_two_words           ; get address range -> mon_addr, mon_end
        jsr     get_byte_skip_ws        ; get fill byte
        pha
        jsr     print_cr                ; print CR
        pla
        ldx     #$00
ofill:
        sta     (mon_addr,x)
        pha
        jsr     check_end               ; check end (before incrementing)
        bcs     odone                   ; at end -> done (byte was written)
        jsr     inc_addr                ; advance to next address
        pla
        jmp     ofill
odone:
        pla
        rts

; ------------------------------------------------------------------------------
; write - W command, copy/move memory
; ------------------------------------------------------------------------------
; W ssss eeee dddd - copy memory ssss-eeee to dddd
; Handles overlapping regions by selecting forward or backward copy.
; ------------------------------------------------------------------------------
write:
        ; get 3 words: mon_src_addr=src, mon_end_addr=end, mon_find_arg=dest
        jsr     get_three_words
        jsr     print_cr                ; print CR
wcopy:
        ; entry point for convert (C command)
        ; compute offset = dest - source into mon_operand
        jsr     compute_offset
        ; X=0 from compute_offset, clear error flag
        stx     mon_scratch2
        ldy     #$02                    ; direction: forward (+1)
        bcc     wr2                     ; if dest < source, copy forward
        ldx     #$02                    ; else copy backward
        ldy     #$00                    ; direction: backward (-1)
; compute dest-end: mon_mode/mon_addr_mode = mon_end_addr + offset
wr2:
        clc
        lda     mon_end_addr
        adc     mon_operand
        sta     mon_mode
        lda     mon_end_addr+1
        adc     mon_operand+1
        sta     mon_addr_mode
        ; copy loop
wrcopy:
        lda     (mon_src_addr,x)        ; load from source
        sta     (mon_find_arg,x)        ; store to dest
        eor     (mon_find_arg,x)        ; verify write
        ora     mon_scratch2
        sta     mon_scratch2            ; accumulate errors
        ; check if done: source >= end?
        lda     mon_src_addr
        cmp     mon_end_addr
        lda     mon_src_addr+1
        sbc     mon_end_addr+1
        bcs     wrdone
        ; advance pointers in selected direction
wradv:
        clc
        lda     mon_src_addr,x
        adc     asm_mode_data,y
        sta     mon_src_addr,x
        lda     mon_src_addr+1,x
        adc     asm_mode_data+1,y
        sta     mon_src_addr+1,x
        txa
        clc
        adc     #$04
        tax
        cmp     #$07
        bcc     wradv
        sbc     #$08
        tax
        bcs     wrcopy                  ; always taken (C set after sbc)
wrdone:
        lda     mon_scratch2
        beq     wrok
        jmp     error                   ; write verification failed
wrok:
        rts

; ------------------------------------------------------------------------------
; compute_offset - compute (mon_operand) = (mon_find_arg) - (mon_src_addr),
; 16-bit
; ------------------------------------------------------------------------------
; Uses ZP,X wrapping: mon_mode+$FE wraps to mon_find_arg, mon_end_addr+$FE to
; mon_src_addr,
; mon_scratch1+$FE to mon_operand.  Relies on contiguous layout in monitor.s.
; Returns: C set if mon_find_arg >= mon_src_addr, X = 0.
; ------------------------------------------------------------------------------
compute_offset:
        sec
        ldx     #$FE
compute_offset_lp:
        ; x=$FE->mon_find_arg, x=$FF->mon_find_arg+1
        lda     mon_mode,x
        ; x=$FE->mon_src_addr, x=$FF->mon_src_addr+1
        sbc     mon_end_addr,x
        ; x=$FE->mon_operand, x=$FF->mon_operand+1
        sta     mon_scratch1,x
        inx
        bne     compute_offset_lp
        rts

; ------------------------------------------------------------------------------
; register - R command, display registers
; ------------------------------------------------------------------------------
; Uses direct indexed access into contiguous mon_regsave block.
; ------------------------------------------------------------------------------
register:
        ldy     #>reghdr
        lda     #<reghdr
        jsr     strout
        ldx     #$3B                    ; ';'
        jsr     print_cr_then_x         ; CR then ';'
        lda     mon_pchsave
        sta     mon_addr+1
        lda     mon_pclsave
        sta     mon_addr
        jsr     print_word              ; print PC
        jsr     print_space             ; space
        ; print SR AK XR YR SP (indices 2..6 of mon_regsave)
        ldy     #$00
reg_lp:
        lda     mon_srsave,y
        jsr     print_hex_byte          ; print hex byte
        jsr     print_space             ; space
        iny
        cpy     #$05
        bne     reg_lp
        lda     mon_srsave              ; print SR as binary
        jmp     print_binary

; ------------------------------------------------------------------------------
; print_binary - print byte in A as binary (space + 8 bits)
; ------------------------------------------------------------------------------
print_binary:
        sta     mon_mode
        lda     #$20
        ldy     #$09
print_binary_lp:
        jsr     monio_chrout
        asl     mon_mode
        lda     #$30
        adc     #$00
        dey
        bne     print_binary_lp
        rts

; ------------------------------------------------------------------------------
; semi - ; command, edit registers
; ------------------------------------------------------------------------------
; Uses direct indexed access into contiguous mon_regsave block.
; ------------------------------------------------------------------------------
semi:
        jsr     get_addr_to_pc          ; get PC
        ldy     #$00
semi_lp:
        jsr     get_char                ; skip separator
        jsr     get_hex_byte            ; get hex byte
        sta     mon_srsave,y
        iny
        cpy     #$05
        bne     semi_lp
        jsr     print_space             ; space
        lda     mon_srsave              ; SR
        jmp     print_binary            ; print as binary

; ------------------------------------------------------------------------------
; go - G command, run from address
; ------------------------------------------------------------------------------
; Uses direct indexed access into contiguous mon_regsave block.
; ------------------------------------------------------------------------------
go:
        jsr     get_opt_addr            ; optionally get address
; --- fall through to restore_and_rti ---

; ------------------------------------------------------------------------------
; exit_monitor / restore_and_rti - X command, exit monitor
; ------------------------------------------------------------------------------
; Restore saved registers and RTI to the interrupted code.
; Also used as the tail of the G command.
; ------------------------------------------------------------------------------
exit_monitor:
restore_and_rti:
        ldx     mon_spsave
        txs
        ; push PCH, PCL, SR, AK, XR, YR in order (indices 0..5)
        ldy     #$00
go_push:
        lda     mon_regsave,y
        pha
        iny
        cpy     #$06
        bne     go_push
        pla                             ; Y
        tay
        pla                             ; X
        tax
        pla                             ; A
        rti

; ------------------------------------------------------------------------------
; load - L command, load Intel HEX data
; ------------------------------------------------------------------------------
load:
        lda     #13
        jsr     monio_chrout
ldnxt:
        jsr     ldget                   ; get char from UART (with echo)
        beq     ldnxt                   ; skip NUL
        cmp     #' '
        beq     ldnxt
        cmp     #13
        beq     ldnxt
        cmp     #10
        beq     ldnxt
        cmp     #27                     ; ESC?
        beq     ldbrk
        cmp     #3                      ; CTRL-C?
        beq     ldbrk
        cmp     #':'                    ; expect ':' at start
        bne     ldeic
        jsr     ldbyt                   ; byte count
        tax
        jsr     ldbyt                   ; address high
        sta     mon_addr+1
        jsr     ldbyt                   ; address low
        sta     mon_addr
        jsr     ldbyt                   ; record type
        beq     lddr                    ; 0 = data record
        cmp     #1                      ; 1 = EOF
        bne     lderi
        ; EOF record
        jsr     ldbyt
        cmp     #$FF                    ; checksum of EOF = $FF
        bne     ldecs
ldeof:
        jmp     ldsync                  ; CR/LF + reset state, rts to caller

        ; data record
lddr:
        clc
        txa                             ; byte count
        adc     mon_addr                ; + addr low
        clc
        adc     mon_addr+1              ; + addr high
        sta     mon_end                 ; running checksum
        ldy     #0
        inx
lddr1:
        dex
        beq     lddr2                   ; all bytes read
        jsr     ldbyt
        sta     (mon_addr),y
        cmp     (mon_addr),y            ; verify
        bne     ldem
        clc
        adc     mon_end
        sta     mon_end
        iny
        bne     lddr1
lddr2:
        jsr     ldbyt                   ; checksum byte
        clc
        adc     mon_end
        bne     ldecs                   ; sum should be 0
        lda     #'+'
        jsr     monio_rawputc            ; echo '+' to terminal
        jsr     ldsync                  ; CR/LF + reset state
        cpy     #0
        bne     ldnxt                   ; more records
        rts                             ; done (already synced)

ldbrk:
        lda     #'B'
        .byte   $2C
lderi:
        lda     #'R'
        .byte   $2C
ldecs:
        lda     #'C'
        .byte   $2C
ldeic:
        lda     #'I'
        .byte   $2C
ldem:
        lda     #'M'
        pha
        jsr     ldsync                  ; CR/LF + reset state
        pla
        jsr     monio_chrout
lderr:
        jmp     error

        ; get hex byte from UART
ldbyt:
        jsr     ldnib                   ; high nibble
        asl
        asl
        asl
        asl
        sta     mon_scratch1
        jsr     ldnib                   ; low nibble
        ora     mon_scratch1
        rts
        ; get hex nibble from UART (skips whitespace)
ldnib:
        jsr     ldget
        beq     ldnib                   ; skip NUL
        cmp     #' '
        beq     ldnib                   ; skip space
        cmp     #13
        beq     ldnib                   ; skip CR
        cmp     #10
        beq     ldnib                   ; skip LF
        jsr     ucase
        cmp     #'0'
        bcc     ldeic
        cmp     #'F'+1
        bcs     ldeic
        cmp     #'9'+1
        bcc     ldnib2
        cmp     #'A'
        bcc     ldeic
        adc     #$08
ldnib2:
        and     #$0F
        rts

; --- read byte from UART with echo --------------------------------------------
ldget:
        jsr     monio_rawgetc            ; blocking read -> A
        cmp     #$20
        bcc     @noecho                 ; don't echo control chars
        pha
        jsr     monio_rawputc            ; echo to terminal
        pla                             ; restore byte (sets N, Z)
        rts
@noecho:
        cmp     #0                      ; set Z for NUL check
        rts

; --- sync terminal after load echoing -----------------------------------------
ldsync:
        lda     #13
        jsr     monio_rawputc
        lda     #10
        jsr     monio_rawputc
        stz     mon_csrcol
        stz     mon_termcol
        rts

; ------------------------------------------------------------------------------
; addsub - ? command, add/subtract/multiply/divide
; ------------------------------------------------------------------------------
addsub:
        jsr     get_word                ; first operand -> mon_addr
        jsr     get_char                ; get operator (+, -, *, /)
        pha                             ; save operator
        jsr     get_word_alt            ; second operand -> mon_end
        pla
        cmp     #'+'
        beq     do_add
        cmp     #'-'
        beq     do_sub
        cmp     #'*'
        beq     do_mul
        cmp     #'/'
        beq     do_div
        jmp     error
do_add:
        clc
        lda     mon_end
        adc     mon_addr
        tax
        lda     mon_end+1
        adc     mon_addr+1
print_value_sec:
        sec
        bcs     print_value_xy          ; always
do_sub:
        sec
        lda     mon_addr
        sbc     mon_end
        tax
        lda     mon_addr+1
        sbc     mon_end+1
        bra     print_value_xy

; --- 16x16 -> 16 unsigned multiply (shift-and-add) ---------------------------
do_mul:
        stz     mon_tmp                 ; product lo = 0
        stz     mon_tmp+1               ; product hi = 0
        ldy     #16                     ; 16 bits
@loop:
        lsr     mon_end+1               ; shift multiplier right
        ror     mon_end
        bcc     @skip                   ; bit was 0, no add
        clc
        lda     mon_tmp
        adc     mon_addr                ; add multiplicand to product
        sta     mon_tmp
        lda     mon_tmp+1
        adc     mon_addr+1
        sta     mon_tmp+1
@skip:
        asl     mon_addr                ; shift multiplicand left
        rol     mon_addr+1
        dey
        bne     @loop
        ldx     mon_tmp                 ; result lo
        lda     mon_tmp+1               ; result hi
        bra     print_value_xy

; --- 16/16 -> 16 unsigned divide (shift-subtract) ----------------------------
do_div:
        lda     mon_end                 ; check for divide by zero
        ora     mon_end+1
        beq     @divzero
        stz     mon_tmp                 ; remainder lo = 0
        stz     mon_tmp+1               ; remainder hi = 0
        ldy     #16                     ; 16 bits
@loop:
        asl     mon_addr                ; shift dividend left into remainder
        rol     mon_addr+1
        rol     mon_tmp
        rol     mon_tmp+1
        sec
        lda     mon_tmp
        sbc     mon_end                 ; trial subtract divisor
        tax
        lda     mon_tmp+1
        sbc     mon_end+1
        bcc     @skip                   ; remainder < divisor, skip
        sta     mon_tmp+1               ; keep subtraction
        stx     mon_tmp
        inc     mon_addr                ; set quotient bit
@skip:
        dey
        bne     @loop
        ldx     mon_addr                ; quotient lo
        lda     mon_addr+1              ; quotient hi
        bra     print_value_xy
@divzero:
        jmp     error

print_value_xy:
        tay
print_value_xa:
        txa

; ------------------------------------------------------------------------------
; print_value - output 16-bit value (A=low, Y=high) as hex, binary, decimal
; ------------------------------------------------------------------------------
print_value:
        pha                             ; save A (low byte)
        jsr     print_cr                ; CR - start output on new line
        pla
        sty     mon_addr+1
        sta     mon_addr
        sty     mon_decval              ; high byte
        sta     mon_decval+1            ; low byte
        php
        lda     #$00
        sta     mon_csrcol
        jsr     erase_eol               ; clear line buffer
        lda     mon_addr+1
        bne     pv_word
        jsr     print_two_spaces        ; 2 spaces (pad for 1-byte value)
        lda     mon_addr
        jsr     print_hex_byte          ; print 2-digit hex
        lda     mon_addr
        jsr     print_binary            ; print as binary
        beq     pv_space                ; always (Z set after print_binary loop)
pv_word:
        jsr     print_word              ; print 4-digit hex
pv_space:
        jsr     print_space             ; space
        plp
        jmp     prtint

; ------------------------------------------------------------------------------
; cmd_hex - $ command, convert hex
; ------------------------------------------------------------------------------
cmd_hex:
        jsr     get_byte_skip_ws        ; get first hex byte
        tax
        ldy     mon_csrcol
        lda     (mon_lineptr),y         ; peek next char in line buffer
        eor     #$20                    ; is it space?
        bne     @twobyte                ; no: 2-byte hex number
        jmp     print_value_sec         ; yes: 1-byte number
@twobyte:
        txa
        tay                             ; high byte = first byte
        jsr     get_hex_byte            ; get second byte
hex_to_value:
        sec
        bcs     print_value             ; always

; ------------------------------------------------------------------------------
; cmd_bin - % command, convert binary
; ------------------------------------------------------------------------------
cmd_bin:
        jsr     skip_spaces             ; skip spaces
        ldy     #$08
bin_bit_loop:
        pha
        jsr     get_char
        cmp     #$31                    ; '1' -> C=1, '0' -> C=0
        pla
        rol                             ; rotate C into bit 0
        dey
        bne     bin_bit_loop
        beq     hex_to_value            ; always (Z set from DEY)

; ------------------------------------------------------------------------------
; cmd_dec - # command, convert decimal
; ------------------------------------------------------------------------------
cmd_dec:
        jsr     skip_spaces             ; skip spaces
        ldx     #$00
        txa
dec_parse_lo:
        stx     mon_addr
        sta     mon_addr+1
        tay
        jsr     monio_chrin
        cmp     #$3A                    ; >= ':'?
        bcs     print_value_xa          ; done (non-digit)
        ; convert '0'-'9' -> 0-9 (C clear from CMP bcs)
        sbc     #$2F
        bcs     dec_parse_hi
        sec
        jmp     print_value_xa          ; invalid
dec_parse_hi:
        sta     mon_end
        ; multiply mon_addr/fb+1 by 10, add mon_end
        asl     mon_addr
        rol     mon_addr+1
        lda     mon_addr+1
        sta     mon_end+1
        lda     mon_addr
        asl
        rol     mon_end+1
        asl
        rol     mon_end+1
        clc
        adc     mon_addr
        php
        clc
        adc     mon_end
        tax
        lda     mon_end+1
        adc     mon_addr+1
        plp
        adc     #$00
        jmp     dec_parse_lo

; ------------------------------------------------------------------------------
; tick - ' command, enter ASCII chars to memory
; ------------------------------------------------------------------------------
tick:
        jsr     get_word                ; get start address
        ldy     #$03
tcae0:
        jsr     monio_chrin
        cmp     #' '
        bne     tstrt
        dey
        bne     tcae0
tloop:
        jsr     monio_chrin
tloop1:
        cmp     #$0D
        beq     tend
        sta     (mon_addr),y
        iny
        cpy     #72
        bcc     tloop
tend:
        rts
tstrt:
        ldy     #0
        jmp     tloop1

; ------------------------------------------------------------------------------
; equals - = command, compare memory
; ------------------------------------------------------------------------------
equals:
        jsr     get_two_words           ; get two addresses
        ldx     #$00
eq_lp:
        lda     (mon_addr,x)
        cmp     (mon_end,x)
        bne     eq_done
        jsr     inc_addr                ; increment mon_addr
        inc     mon_end
        bne     eq_lp
        inc     mon_end+1
        bne     eq_lp
eq_done:
        jsr     print_cr                ; CR
        jsr     print_space             ; space
        jmp     print_word              ; print address of first difference

; ------------------------------------------------------------------------------
; find - F command
; ------------------------------------------------------------------------------
find:
        lda     #$FF
        ldx     #$04
f_init:
        sta     mon_swap_tmp,x          ; set mon_addr-mon_end+1 to $FF
        dex
        bne     f_init
        jsr     get_char
        ldx     #$05
f_chk:
        cmp     fscmd-1,x
        beq     f_sub                   ; found sub-command (A/Z/I/R/T)
        dex
        bne     f_chk

        ; plain F: find byte sequence
        stx     mon_find_arg+1          ; X=0, store byte index
f_byte:
        jsr     find_get_pair           ; get search data (2 nibbles + masks)
        inx
        jsr     monio_chrin
        cmp     #' '
        beq     f_byte
        cmp     #$2C                    ; ','
        bne     f_range
        jsr     get_two_words
f_range:
        jsr     print_cr
f_scan:
        ldy     mon_find_arg+1          ; byte count - 1
f_cmp:
        lda     (mon_addr),y
        jsr     find_compare            ; masked compare
        bne     f_next
        dey
        bpl     f_cmp
        ; match found
        jsr     print_word              ; print address
        jsr     print_space
        ldy     mon_csrcol
        cpy     #76
        bcc     f_next
        jsr     page_pause
        jsr     print_cr
f_next:
        jsr     inc_check_end
        bcc     f_scan
        ldy     #$27
        rts

        ; F sub-command (AZIRT)
f_sub:
        lda     find_mode_flags-1,x
        sta     mon_find_arg
        lda     find_data_len-1,x
        sta     mon_find_arg+1
        tax
        beq     f_sub2                  ; no data bytes to read
f_sub1:
        jsr     find_get_pair
        dex
        bne     f_sub1
f_sub2:
        jsr     get_two_words
f_sub3:
        jsr     decode_opcode           ; decode opcode
        jsr     read_operand            ; read operand
        lda     mon_find_arg
        bit     mon_addr_mode
        bne     f_sub5
        tay
        bne     f_sub7                  ; flag set but no match
        lda     mon_opcode_idx
        bne     f_sub7
        beq     f_sub6                  ; table byte found
f_sub5:
        ldy     mon_find_arg+1
f_sub5a:
        lda     mon_opcode_idx,y
        jsr     find_compare
        bne     f_sub7
        dey
        bne     f_sub5a
f_sub6:
        sty     mon_mode
        jsr     disasm_one              ; disassemble match
        jsr     page_pause              ; pause/stop
f_sub6a:
        jsr     check_end
        bcc     f_sub3
        rts
f_sub7:
        jsr     advance_by_len          ; advance past instruction
        beq     f_sub6a                 ; always (Z set from advance_by_len)

; ------------------------------------------------------------------------------
; find_get_pair - get two hex nibbles with wildcard masks
; ------------------------------------------------------------------------------
find_get_pair:
        jsr     find_get_nibble         ; get high nibble + mask
        sta     mon_findlmsk,x          ; store high nibble mask
        lda     mon_labels,x            ; move high nibble value
        sta     mon_findhi,x
        ; fall through to get low nibble

; ------------------------------------------------------------------------------
; find_get_nibble - get one hex nibble with wildcard
; ------------------------------------------------------------------------------
find_get_nibble:
        jsr     get_char
        ldy     #$0F                    ; mask = $0F (all bits matter)
        cmp     #'*'                    ; wildcard?
        bne     find_convert_nib
        ldy     #$00                    ; mask = $00 (no bits matter)
find_convert_nib:
        jsr     hex_char_to_val         ; convert to nibble
        sta     mon_labels,x
        tya
        sta     mon_findhmsk,x
        rts

; ------------------------------------------------------------------------------
; find_compare - masked compare byte A with search data at index Y
; ------------------------------------------------------------------------------
find_compare:
        sta     mon_scratch1
        lsr
        lsr
        lsr
        lsr
        eor     mon_findhi,y
        and     mon_findlmsk,y
        and     #$0F
        bne     find_compare_rts
        lda     mon_scratch1
        eor     mon_labels,y
        and     mon_findhmsk,y
        and     #$0F
find_compare_rts:
        rts

; ------------------------------------------------------------------------------
; check_relocate - check if address A:X is in range mon_src_addr..mon_end_addr,
; relocate if so
; ------------------------------------------------------------------------------
check_relocate:
        cmp     mon_end_addr+1
        bne     cr_no_reloc1
        cpx     mon_end_addr
cr_no_reloc1:
        bcs     cr_done                 ; >= end -> no relocation
        cmp     mon_src_addr+1
        bne     cr_no_reloc2
        cpx     mon_src_addr
cr_no_reloc2:
        bcc     cr_done                 ; < start -> no relocation
        ; in range: add offset
        sta     mon_scratch1
        txa
        clc
        adc     mon_operand
        tax
        lda     mon_scratch1
        adc     mon_operand+1
cr_done:
        rts

; ------------------------------------------------------------------------------
; convert_addrs - convert addresses in range
; ------------------------------------------------------------------------------
convert_addrs:
        jsr     get_three_words         ; get src_start, src_end, dest
        jsr     get_two_words           ; get scan range
        jsr     print_cr
cv_loop_offset:
        jsr     compute_offset          ; compute offset
cv_loop:
        jsr     decode_opcode           ; decode opcode
        iny
        lda     #$10
        bit     mon_addr_mode
        beq     cv_check_abs            ; not relative branch
        ; relative branch: check and relocate
        ldx     mon_addr
        lda     mon_addr+1
        jsr     check_relocate          ; check instruction address
        stx     mon_mode
        lda     (mon_addr),y            ; branch offset
        sta     mon_scratch2
        jsr     branch_target           ; compute target
        ldy     #$01
        jsr     check_relocate          ; check target address
        dex
        txa
        clc
        sbc     mon_mode
        sta     (mon_addr),y            ; store new offset
        eor     mon_scratch2
        bpl     cv_advance
        jsr     print_cr                ; sign changed: warn
        jsr     print_word
cv_check_abs:
        bit     mon_addr_mode
        bpl     cv_advance              ; not absolute
        ; absolute address: check and relocate
        lda     (mon_addr),y            ; operand low
        tax
        iny
        lda     (mon_addr),y            ; operand high
        jsr     check_relocate
        sta     (mon_addr),y            ; store relocated high
        txa
        dey
        sta     (mon_addr),y            ; store relocated low
cv_advance:
        jsr     advance_by_len          ; advance past instruction
        jsr     check_end               ; check end
        bcc     cv_loop
        rts

; ------------------------------------------------------------------------------
; move - V command, convert addresses
; ------------------------------------------------------------------------------
move:
        jmp     convert_addrs

; ------------------------------------------------------------------------------
; convert - C command, convert + copy
; ------------------------------------------------------------------------------
convert:
        jsr     convert_addrs           ; convert addresses
        jmp     wcopy                   ; then copy memory

; ------------------------------------------------------------------------------
; default_irq - default IRQ handler (no-op, just return)
; ------------------------------------------------------------------------------
; Called via jmp (mon_irq_vec) with Y, X, A on stack.
; ------------------------------------------------------------------------------
default_irq:
        ply
        plx
        pla
        rti

; ------------------------------------------------------------------------------
; trace - T command (TW/TQ/TS/TB)
; ------------------------------------------------------------------------------
; Uses BRK-injection single-stepping: decode the instruction at the user
; PC, compute the address(es) of the next instruction, place BRK ($00)
; there, then execute user code via RTI.  The BRK handler restores the
; patched bytes, saves state, and displays the trace line.
;
; TW = trace walk (interactive, press key for next step)
; TQ = trace quick (single step, show registers)
; TS = trace stop (run to address, then break)
; TB = trace break (run to address, stop after N hits)
; ------------------------------------------------------------------------------
trace:
        jsr     monio_chrin
        jsr     ucase
        cmp     #'W'
        beq     tracew
        cmp     #'Q'
        beq     traceq
        cmp     #'S'
        beq     traces
        cmp     #'B'
        beq     traceb
        jmp     error

; --- TS xxxx - run until PC reaches xxxx (count=1) ----------------------------
traces:
        jsr     get_word                ; breakpoint address -> mon_addr
        lda     #1
        bne     ts_go                   ; always

; --- TB xxxx nn - run, stop after nn-th hit at xxxx ---------------------------
traceb:
        jsr     get_word                ; breakpoint address -> mon_addr
        jsr     get_byte_skip_ws        ; hit count -> A

ts_go:
        sta     mon_tracecnt
        ; copy breakpoint address
        lda     mon_addr
        sta     mon_traceaddr
        lda     mon_addr+1
        sta     mon_traceaddr+1
        ; init user SR if needed
        php
        pla
        and     #$EF                    ; clear BRK flag
        sta     mon_srsave

        ; save original byte at breakpoint, inject BRK
        lda     mon_traceaddr+1
        sta     mon_src_addr+1
        lda     mon_traceaddr
        sta     mon_src_addr
        ldy     #0
        lda     (mon_src_addr),y
        sta     mon_bp1_save
        lda     #$00                    ; BRK
        sta     (mon_src_addr),y

        ; set BRK vector -> tsint, save SP, RTI to user code
        tsx
        stx     mon_spsave
        jsr     print_cr                ; CR

        lda     #<tsint
        sta     mon_brk_vec
        lda     #>tsint
        sta     mon_brk_vec+1

        ldx     mon_spsave
        txs
        lda     mon_pchsave
        pha
        lda     mon_pclsave
        pha
        lda     mon_srsave
        and     #$EF
        pha
        lda     mon_arsave
        ldx     mon_xrsave
        ldy     mon_yrsave
        ; -> user code (runs until BRK at traceaddr)
        rti

traceq:
        php
        pla
        and     #$EF                    ; clear BRK flag
        sta     mon_srsave              ; use current SR as user SR
        lda     #$40                    ; TQ mode
        bne     trace_go                ; always

tracew:
        php
        pla
        and     #$EF                    ; clear BRK flag
        sta     mon_srsave              ; use current SR as user SR
        lda     #$80                    ; TW mode

trace_go:
        sta     mon_tracemode
        tsx
        stx     mon_spsave
        jsr     get_opt_addr            ; optionally get start PC
        lda     mon_tracemode
        bpl     @tq                     ; TQ -> just CR, then launch
        ; TW -> show initial state before first step
        jmp     tw_walk
@tq:
        jsr     print_cr                ; TQ -> CR

; ------------------------------------------------------------------------------
; tl_launch - inject BRK(s) and execute one user instruction via RTI
; ------------------------------------------------------------------------------
tl_launch:
        ; --- decode instruction at user PC ---
        lda     mon_pclsave
        sta     mon_addr
        lda     mon_pchsave
        sta     mon_addr+1
        ; decode -> mon_inst_len=length, mon_opcode_idx=index
        jsr     decode_opcode

        ; --- classify by raw opcode ---
        ldy     #$00
        lda     (mon_addr),y            ; raw opcode byte

        ; check BRK ($00)
        beq     tl_is_brk

        ; check RTS ($60)
        cmp     #$60
        beq     tl_is_rts

        ; check RTI ($40)
        cmp     #$40
        beq     tl_is_rti

        ; check JMP abs ($4C)
        cmp     #$4C
        beq     tl_is_jmpabs

        ; check JSR ($20)
        cmp     #$20
        beq     tl_is_jmpabs            ; same logic - target is operand word

        ; check JMP (ind) ($6C)
        cmp     #$6C
        bne     :+
        jmp     tl_is_jmpind
:
        ; check JMP (abs,X) ($7C) - 65C02
        cmp     #$7C
        bne     :+
        jmp     tl_is_jmpindx
:
        ; check BRA ($80) - 65C02 unconditional branch
        cmp     #$80
        bne     :+
        jmp     tl_is_bra
:
        ; check conditional branches: $10,$30,$50,$70,$90,$B0,$D0,$F0
        ; all have low nibble=0 and bit 4 set
        and     #$1F
        cmp     #$10
        bne     tl_linear
        jmp     tl_is_branch

        ; --- linear instruction: BRK at PC + length ---
tl_linear:
        lda     #1
        sta     mon_bp_count
        ; compute PC + mon_inst_len
        clc
        lda     mon_pclsave
        adc     mon_inst_len
        sta     mon_bp1_addr
        lda     mon_pchsave
        adc     #0
        sta     mon_bp1_addr+1
        jmp     tl_inject

        ; --- BRK in user code: just let it fire, but decrement PC ---
tl_is_brk:
        ; BRK pushes PC+2, BRK handler adjusts.  Just run it.
        lda     #0
        sta     mon_bp_count            ; no breakpoints needed
        jmp     tl_run

        ; --- RTS: read return address from user stack ---
tl_is_rts:
        lda     #1
        sta     mon_bp_count
        ; user SP is in mon_spsave; stack at $0100+SP
        ; RTS pops PCL, PCH and adds 1
        ldx     mon_spsave
        lda     $0101,x                 ; PCL
        clc
        adc     #1
        sta     mon_bp1_addr
        lda     $0102,x                 ; PCH
        adc     #0
        sta     mon_bp1_addr+1
        jmp     tl_inject

        ; --- RTI: read return address from user stack ---
tl_is_rti:
        lda     #1
        sta     mon_bp_count
        ; RTI pops SR, PCL, PCH (does NOT add 1)
        ldx     mon_spsave
        lda     $0102,x                 ; PCL (past SR)
        sta     mon_bp1_addr
        lda     $0103,x                 ; PCH
        sta     mon_bp1_addr+1
        jmp     tl_inject

        ; --- JMP absolute / JSR: target is operand word ---
tl_is_jmpabs:
        lda     #1
        sta     mon_bp_count
        ldy     #1
        lda     (mon_addr),y            ; target low
        sta     mon_bp1_addr
        iny
        lda     (mon_addr),y            ; target high
        sta     mon_bp1_addr+1
        jmp     tl_inject

        ; --- JMP (ind): target is *(operand word) ---
tl_is_jmpind:
        lda     #1
        sta     mon_bp_count
        ; read the pointer address from the operand
        ldy     #1
        lda     (mon_addr),y            ; pointer low
        sta     mon_src_addr
        iny
        lda     (mon_addr),y            ; pointer high
        sta     mon_src_addr+1
        ; read the target from the pointer
        ldy     #0
        lda     (mon_src_addr),y
        sta     mon_bp1_addr
        iny
        lda     (mon_src_addr),y
        sta     mon_bp1_addr+1
        jmp     tl_inject

        ; --- JMP (abs,X): target is *((operand word)+X) ---
tl_is_jmpindx:
        lda     #1
        sta     mon_bp_count
        ; read the pointer address from the operand, add user X
        ldy     #1
        clc
        lda     (mon_addr),y            ; pointer low
        adc     mon_xrsave              ; add user X
        sta     mon_src_addr
        iny
        lda     (mon_addr),y            ; pointer high
        adc     #0
        sta     mon_src_addr+1
        ; read the target from the pointer
        ldy     #0
        lda     (mon_src_addr),y
        sta     mon_bp1_addr
        iny
        lda     (mon_src_addr),y
        sta     mon_bp1_addr+1
        jmp     tl_inject

        ; --- BRA (unconditional relative branch): target only ---
tl_is_bra:
        lda     #1
        sta     mon_bp_count
        jsr     branch_target           ; compute branch target -> X=lo, A=hi
        stx     mon_bp1_addr
        sta     mon_bp1_addr+1
        jmp     tl_inject

        ; --- conditional branch: BRK at both PC+2 and branch target ---
tl_is_branch:
        lda     #2
        sta     mon_bp_count
        ; fall-through address = PC + 2
        clc
        lda     mon_pclsave
        adc     #2
        sta     mon_bp1_addr
        lda     mon_pchsave
        adc     #0
        sta     mon_bp1_addr+1
        ; branch target
        jsr     branch_target           ; compute branch target -> X=lo, A=hi
        stx     mon_bp2_addr
        sta     mon_bp2_addr+1
        ; fall through to tl_inject

; --- inject BRK(s) at computed address(es) ------------------------------------
tl_inject:
        ; save original byte at bp1 and write BRK
        lda     mon_bp1_addr
        sta     mon_src_addr
        lda     mon_bp1_addr+1
        sta     mon_src_addr+1
        ldy     #0
        lda     (mon_src_addr),y
        sta     mon_bp1_save
        lda     #$00                    ; BRK
        sta     (mon_src_addr),y

        ; if 2 breakpoints, inject second
        lda     mon_bp_count
        cmp     #2
        bne     tl_run

        lda     mon_bp2_addr
        sta     mon_src_addr
        lda     mon_bp2_addr+1
        sta     mon_src_addr+1
        lda     (mon_src_addr),y
        sta     mon_bp2_save
        lda     #$00                    ; BRK
        sta     (mon_src_addr),y

; --- set BRK vector and execute user code -------------------------------------
tl_run:
        ; set BRK vector to our handler
        lda     #<twint
        sta     mon_brk_vec
        lda     #>twint
        sta     mon_brk_vec+1

        ; restore user registers and RTI
        ldx     mon_spsave
        txs
        lda     mon_pchsave
        pha
        lda     mon_pclsave
        pha
        lda     mon_srsave
        and     #$EF                    ; clear BRK flag in user SR
        pha
        lda     mon_arsave
        ldx     mon_xrsave
        ldy     mon_yrsave
        rti                             ; -> user code (will hit our BRK)

; ------------------------------------------------------------------------------
; twint - BRK handler for trace single-step
; ------------------------------------------------------------------------------
; Entered via jmp (mon_brk_vec) with Y, X, A on stack (from _irq_int).
; Hardware pushed SR, PCL, PCH.  Stack: Y, X, A, SR, PCL, PCH (top->bot).
; BRK pushes PC+2 on 65C02, so we must adjust PC back by 2.
; ------------------------------------------------------------------------------
twint:
        cld
        ; pop saved registers from stack
        ldx     #$05
tw_save:
        pla
        ; [5]=Y, [4]=X, [3]=A, [2]=SR, [1]=PCL, [0]=PCH
        sta     mon_regsave,x
        dex
        bpl     tw_save
        tsx
        stx     mon_spsave

        ; adjust PC: BRK pushed PC+2, we want PC of the BRK opcode
        sec
        lda     mon_pclsave
        sbc     #2
        sta     mon_pclsave
        bcs     @nolo
        dec     mon_pchsave
@nolo:

        ; restore original bytes at breakpoint location(s)
        jsr     tw_restore

        ; restore BRK vector to mon_brk
        lda     #<mon_brk
        sta     mon_brk_vec
        lda     #>mon_brk
        sta     mon_brk_vec+1

        ; if user code hit its own BRK (not ours), just exit
        ; check: does the BRK PC match one of our breakpoints?
        lda     mon_bp_count
        beq     tw_user_brk             ; we injected 0 BRKs -> user's own BRK
        jsr     tw_check_bp
        bne     tw_user_brk             ; PC doesn't match any of our BPs

        ; check trace mode
        lda     mon_tracemode
        bmi     tw_walk                 ; $80 = TW mode
        cmp     #$20
        beq     ts_reinject             ; $20 = TS/TB continue after single-step

        ; TQ mode ($40) - single step done, show and exit
        jmp     tw_exit

tw_user_brk:
        ; user code hit a BRK - show registers and exit
        jmp     tw_exit

; --- TS/TB: re-inject BRK at traceaddr and resume full-speed ------------------
ts_reinject:
        lda     mon_traceaddr
        sta     mon_src_addr
        lda     mon_traceaddr+1
        sta     mon_src_addr+1
        ldy     #0
        lda     (mon_src_addr),y
        sta     mon_bp1_save            ; save current byte (may have changed)
        lda     #$00
        sta     (mon_src_addr),y        ; inject BRK

        ; set BRK vector -> tsint, resume user code
        lda     #<tsint
        sta     mon_brk_vec
        lda     #>tsint
        sta     mon_brk_vec+1

        ldx     mon_spsave
        txs
        lda     mon_pchsave
        pha
        lda     mon_pclsave
        pha
        lda     mon_srsave
        and     #$EF
        pha
        lda     mon_arsave
        ldx     mon_xrsave
        ldy     mon_yrsave
        rti                             ; -> user code (full speed until BRK)

tw_walk:
        ; TW mode - display trace line (registers + disassembly)
        jsr     tw_display
        ; wait for keypress
tw_key:
        jsr     monio_getin
        beq     tw_key
        ; check for STOP (ESC/Ctrl-C)
        jsr     monio_stop
        beq     tw_exit                 ; Z set = stop pressed
        ; any other key -> step next instruction
        jmp     tl_launch

tw_exit:
        ; restore BRK vector to mon_brk (in case not already done)
        lda     #<mon_brk
        sta     mon_brk_vec
        lda     #>mon_brk
        sta     mon_brk_vec+1
        jsr     print_cr
        lda     #'R'
        jmp     cmdexec                 ; show registers, back to mainloop

; ------------------------------------------------------------------------------
; tsint - BRK handler for TS/TB (breakpoint run)
; ------------------------------------------------------------------------------
; Entered via jmp (mon_brk_vec) with Y, X, A on stack (from _irq_int).
; ------------------------------------------------------------------------------
tsint:
        cld
        ldx     #$05
@save:
        pla
        sta     mon_regsave,x
        dex
        bpl     @save
        tsx
        stx     mon_spsave

        ; adjust PC: BRK pushed PC+2
        sec
        lda     mon_pclsave
        sbc     #2
        sta     mon_pclsave
        bcs     @nolo
        dec     mon_pchsave
@nolo:
        ; restore original byte at breakpoint
        lda     mon_traceaddr
        sta     mon_src_addr
        lda     mon_traceaddr+1
        sta     mon_src_addr+1
        ldy     #0
        lda     mon_bp1_save
        sta     (mon_src_addr),y

        ; restore BRK vector to mon_brk
        lda     #<mon_brk
        sta     mon_brk_vec
        lda     #>mon_brk
        sta     mon_brk_vec+1

        ; check if PC matches our breakpoint
        lda     mon_pclsave
        cmp     mon_traceaddr
        bne     @not_ours
        lda     mon_pchsave
        cmp     mon_traceaddr+1
        bne     @not_ours

        ; it's our breakpoint - decrement hit counter
        dec     mon_tracecnt
        beq     tw_exit                 ; counter reached 0 -> show regs, exit

        ; counter not zero - single-step past BP, then re-inject
        ; tl_launch will decode the instruction at PC, inject BRK at
        ; the next instruction(s), and RTI. twint will fire after that
        ; one instruction executes. We set tracemode=$20 so twint knows
        ; to re-inject our TS/TB breakpoint and resume full-speed.
        lda     #$20
        sta     mon_tracemode
        jmp     tl_launch

@not_ours:
        ; user's own BRK - show regs and exit
        jmp     tw_exit

; ------------------------------------------------------------------------------
; tw_restore - restore original bytes at breakpoint location(s)
; ------------------------------------------------------------------------------
tw_restore:
        lda     mon_bp_count
        beq     @done
        ; restore bp1
        lda     mon_bp1_addr
        sta     mon_src_addr
        lda     mon_bp1_addr+1
        sta     mon_src_addr+1
        lda     mon_bp1_save
        ldy     #0
        sta     (mon_src_addr),y
        ; restore bp2 if needed
        lda     mon_bp_count
        cmp     #2
        bne     @done
        lda     mon_bp2_addr
        sta     mon_src_addr
        lda     mon_bp2_addr+1
        sta     mon_src_addr+1
        lda     mon_bp2_save
        sta     (mon_src_addr),y
@done:
        rts

; ------------------------------------------------------------------------------
; tw_check_bp - check if current PC matches one of our breakpoints
; ------------------------------------------------------------------------------
; Returns Z=1 if match (our BP), Z=0 if no match (user BRK).
; ------------------------------------------------------------------------------
tw_check_bp:
        ; check bp1
        lda     mon_pclsave
        cmp     mon_bp1_addr
        bne     @try2
        lda     mon_pchsave
        cmp     mon_bp1_addr+1
        beq     @match
@try2:
        lda     mon_bp_count
        cmp     #2
        bne     @nomatch
        ; check bp2
        lda     mon_pclsave
        cmp     mon_bp2_addr
        bne     @nomatch
        lda     mon_pchsave
        cmp     mon_bp2_addr+1
        beq     @match
@nomatch:
        lda     #1                      ; Z=0
        rts
@match:
        lda     #0                      ; Z=1
        rts

; ------------------------------------------------------------------------------
; tw_display - print trace line: registers + disassembly
; ------------------------------------------------------------------------------
tw_display:
        jsr     print_cr                ; CR
        ; set mon_addr to address of mon_regsave for indirect read
        lda     #<mon_regsave
        sta     mon_addr
        lda     #>mon_regsave
        sta     mon_addr+1
        jsr     print_space             ; space
        ldy     #$00
tw_regs:
        lda     (mon_addr),y
        jsr     print_hex_byte          ; print hex byte
        iny
        cpy     #$07                    ; 7 bytes: PCH,PCL,SR,AK,XR,YR,SP
        beq     tw_dis
        cpy     #$02                    ; after PCH+PCL, start spacing
        bcc     tw_regs                 ; y=1: no space between PCH and PCL
        jsr     print_space             ; space
        bne     tw_regs                 ; always
tw_dis:
        ; set mon_addr to user PC for disassembly
        lda     mon_pclsave
        sta     mon_addr
        lda     mon_pchsave
        sta     mon_addr+1
        jsr     print_two_spaces        ; two spaces
        jsr     decode_opcode           ; decode opcode at (mon_addr)
        jmp     print_instruction       ; print instruction, return to caller

; ------------------------------------------------------------------------------
; save_brk_vec - save BRK vector
; ------------------------------------------------------------------------------
save_brk_vec:
        lda     mon_brk_vec
        ldx     mon_brk_vec+1
        sta     mon_irqsave
        stx     mon_irqsave+1
        rts

; ------------------------------------------------------------------------------
; restore_brk_vec - restore BRK vector from saved
; ------------------------------------------------------------------------------
restore_brk_vec:
        lda     mon_irqsave
        ldx     mon_irqsave+1
        sta     mon_brk_vec
        stx     mon_brk_vec+1
        rts

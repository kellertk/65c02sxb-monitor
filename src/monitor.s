; ==============================================================================
; monitor.s - monitor variable allocations for ca65/ld65
; ==============================================================================
; All zero-page and BSS variables needed by the machine-language monitor.
; Contiguous groups are used where indexed addressing spans adjacent locations.
; ------------------------------------------------------------------------------

        .setcpu "65C02"

; ------------------------------------------------------------------------------
; Exports - zero page
; ------------------------------------------------------------------------------
        .exportzp mon_termcol, mon_stopflag, mon_lastrecv, mon_rowlimit
        .exportzp mon_msgflag, mon_tmpbuf, mon_kbdbuf, mon_kbdcnt
        .exportzp mon_lastcol, mon_crflag, mon_lineptr, mon_csrcol
        .exportzp mon_csrrow, mon_lastprnt
        .exportzp mon_page_cnt, mon_tmp
        .exportzp mon_decval
        .exportzp mon_work, mon_src_addr, mon_end_addr, mon_find_arg
        .exportzp mon_mode, mon_addr_mode, mon_cmd_char, mon_opcode_idx
        .exportzp mon_operand
        .exportzp mon_scratch1, mon_scratch2, mon_inst_len, mon_asm_col
        .exportzp mon_strptr, mon_scrollptr
        .exportzp mon_addrs, mon_swap_tmp, mon_addr, mon_end, mon_repeat

; ------------------------------------------------------------------------------
; Exports - BSS
; ------------------------------------------------------------------------------
        .export mon_irq_vec, mon_brk_vec
        .export mon_membot, mon_memtop
        .export mon_regsave, mon_pchsave, mon_pclsave
        .export mon_srsave, mon_arsave, mon_xrsave, mon_yrsave, mon_spsave
        .export mon_commaflag, mon_irqsave
        .export mon_tracemode, mon_traceaddr, mon_tracecnt
        .export mon_bp1_addr, mon_bp1_save, mon_bp2_addr, mon_bp2_save
        .export mon_bp_count
        .export mon_labels, mon_findhi, mon_findhmsk, mon_findlmsk
        .export mon_linebuf

; ------------------------------------------------------------------------------
; Constants
; ------------------------------------------------------------------------------
        .include "mon_const.inc"

; ------------------------------------------------------------------------------
;                             ZEROPAGE
; ------------------------------------------------------------------------------
        .segment "ZEROPAGE"

; ------------------------------------------------------------------------------
; terminal I/O state
; ------------------------------------------------------------------------------
mon_termcol:    .res 1          ; terminal cursor column
mon_stopflag:   .res 1          ; stop-key flag ($7F = pressed)
mon_lastrecv:   .res 1          ; previous received char
mon_rowlimit:   .res 1          ; rows currently in buffer
mon_msgflag:    .res 1          ; message output flag
mon_tmpbuf:     .res 1          ; temporary byte
mon_kbdbuf:     .res 1          ; single-char keyboard buffer
mon_kbdcnt:     .res 1          ; keyboard buffer count (0 or 1)
mon_lastcol:    .res 1          ; last cursor column for input
mon_crflag:     .res 1          ; CR flag / ESC flag (shared)
mon_lineptr:    .res 2          ; pointer to current line in buffer
mon_csrcol:     .res 1          ; current cursor column
mon_csrrow:     .res 1          ; current cursor row
mon_lastprnt:   .res 1          ; last character printed

; ------------------------------------------------------------------------------
; MON_BRK general-purpose working area
; ------------------------------------------------------------------------------

mon_page_cnt:   .res 1          ; page line counter
mon_tmp:        .res 2          ; temp (multiply/divide scratch)

; --- decimal print value ---
mon_decval:     .res 2          ; 16-bit value for PRTINT

; --- MON_BRK work block (12 bytes, MUST be contiguous)
; --------------------------
; Indexed via  lda mon_work,x  etc.
mon_work:
mon_src_addr:   .res 2          ; source/temp address
mon_end_addr:   .res 2          ; end/temp address
mon_find_arg:   .res 2          ; find arg / asm mnemonic
mon_mode:       .res 1          ; mode flags
mon_addr_mode:  .res 1          ; addressing-mode byte
mon_cmd_char:   .res 1          ; command character
mon_opcode_idx: .res 1          ; opcode table index
mon_operand:    .res 2          ; operand word

; --- temp / assembler state (4 bytes) ---
mon_scratch1:     .res 1        ; temp
mon_scratch2:     .res 1        ; temp / direction flag
mon_inst_len:   .res 1          ; instruction length
mon_asm_col:    .res 1          ; assembler column

; --- string pointer ---
mon_strptr:     .res 2          ; string output pointer

; --- scroll temp pointer ---
mon_scrollptr:  .res 2          ; scroll src pointer

; --- address-pointer block (6 bytes, MUST be contiguous) ---------------------
; Indexed via  lda mon_addrs+1,x  etc.
mon_addrs:
mon_swap_tmp:   .res 1          ; swap temp
mon_addr:       .res 2          ; current address pointer
mon_end:        .res 2          ; end address pointer
mon_repeat:     .res 1          ; repetition counter

; ------------------------------------------------------------------------------
;                                BSS
; ------------------------------------------------------------------------------
        .segment "BSS"

; --- software interrupt vectors ---
mon_irq_vec:    .res 2          ; IRQ vector
mon_brk_vec:    .res 2          ; BRK vector

; --- memory bounds ---
mon_membot:     .res 2          ; bottom-of-memory
mon_memtop:     .res 2          ; top-of-memory

; --- saved registers (7 bytes, MUST be contiguous) ---------------------------
; Indexed via  lda mon_regsave,x  (x = 0..6)
mon_regsave:
mon_pchsave:    .res 1          ; saved PC high
mon_pclsave:    .res 1          ; saved PC low
mon_srsave:     .res 1          ; saved status register
mon_arsave:     .res 1          ; saved accumulator
mon_xrsave:     .res 1          ; saved X register
mon_yrsave:     .res 1          ; saved Y register
mon_spsave:     .res 1          ; saved stack pointer

; --- trace / debug state ---
mon_commaflag:  .res 1          ; comma-command flag
mon_irqsave:    .res 2          ; saved IRQ vector
mon_tracemode:  .res 1          ; trace mode flag
mon_traceaddr:  .res 2          ; trace break address
mon_tracecnt:   .res 1          ; trace break counter
mon_bp1_addr:   .res 2          ; breakpoint 1 address
mon_bp1_save:   .res 1          ; saved byte at bp1
mon_bp2_addr:   .res 2          ; breakpoint 2 address
mon_bp2_save:   .res 1          ; saved byte at bp2
mon_bp_count:   .res 1          ; number of active breakpoints (1 or 2)

; --- find / label buffers (192 bytes, contiguous) ----------------------------
; Accessed at offsets 0, 48, 96, 144 via absolute indexed addressing
mon_labels:     .res 48         ; label storage
mon_findhi:     .res 48         ; find high nibbles
mon_findhmsk:   .res 48         ; find high-nibble mask
mon_findlmsk:   .res 48         ; find low-nibble mask

; --- line buffer (screen buffer) ---
mon_linebuf:    .res NUMCOLS * NUMROWS

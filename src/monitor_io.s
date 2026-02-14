; ==============================================================================
; monitor_io.s - terminal I/O for WDC 65C02 SXB
; ==============================================================================
; Line-buffered terminal I/O through FT245RL (usb.s).  Provides chrin/chrout
; (line-buffered), getin/stop (keyboard polling), and rawgetc/rawputc
; (unbuffered byte I/O for the load command).
; ------------------------------------------------------------------------------

        .setcpu "65C02"

; ------------------------------------------------------------------------------
; Exports
; ------------------------------------------------------------------------------
        .export monio_chrin, monio_chrout, monio_stop, monio_getin
        .export monio_rawgetc, monio_rawputc
        .export monio_rw_membot, monio_rw_memtop, monio_msgflag
        .export monio_iostub, monio_stub
        .export proccr, rowdn, rowup, scrl, clrl, clrlb
        .export preol, prline, chkcol
        .export mapchr
        .export monio_reset

; ------------------------------------------------------------------------------
; Imports - USB driver
; ------------------------------------------------------------------------------
        .import _usb_putc, _usb_getc, _usb_getc_nb

; ------------------------------------------------------------------------------
; Imports - monitor variables (zero-page)
; ------------------------------------------------------------------------------
        .importzp mon_termcol, mon_stopflag, mon_lastrecv, mon_rowlimit
        .importzp mon_msgflag, mon_tmpbuf, mon_kbdbuf, mon_kbdcnt
        .importzp mon_lastcol, mon_crflag, mon_lineptr, mon_csrcol
        .importzp mon_csrrow, mon_lastprnt
        .importzp mon_scrollptr

; ------------------------------------------------------------------------------
; Imports - monitor variables (BSS)
; ------------------------------------------------------------------------------
        .import mon_membot, mon_memtop
        .import mon_linebuf
        .include "mon_const.inc"

; ------------------------------------------------------------------------------
;                                CODE
; ------------------------------------------------------------------------------
        .segment "CODE"

; ------------------------------------------------------------------------------
; get/set memory bounds
; ------------------------------------------------------------------------------
.proc monio_rw_membot
        bcc     @set
        ldx     mon_membot
        ldy     mon_membot+1
        rts
@set:
        stx     mon_membot
        sty     mon_membot+1
        rts
.endproc

.proc monio_rw_memtop
        bcc     @set
        ldx     mon_memtop
        ldy     mon_memtop+1
        rts
@set:
        stx     mon_memtop
        sty     mon_memtop+1
        rts
.endproc

; ------------------------------------------------------------------------------
; set message flag
; ------------------------------------------------------------------------------
.proc monio_msgflag
        sta     mon_msgflag
        rts

.endproc

; ------------------------------------------------------------------------------
; I/O stubs
; ------------------------------------------------------------------------------
.proc monio_iostub
        lda     #$05            ; "device not present"
        sec
.endproc

.proc monio_stub
        rts
.endproc

; ------------------------------------------------------------------------------
; STOP
; ------------------------------------------------------------------------------
; Test stop key - Z set if stop was pressed.
.proc monio_stop
        lda     mon_stopflag
        cmp     #$7F
        beq     @stopped
        jsr     getin1          ; poll USB for ESC/CTRL-C
        beq     @check
        sta     mon_kbdbuf
        inc     mon_kbdcnt
@check:
        lda     mon_stopflag
        cmp     #$7F
        bne     @done
@stopped:
        lda     #$FF
        sta     mon_stopflag    ; clear stop flag
        lda     #$7F
        cmp     #$7F            ; set Z
        rts
@done:
        lda     #$FF            ; clear Z
        rts

.endproc

; ------------------------------------------------------------------------------
; GETIN
; ------------------------------------------------------------------------------
; Get input character - 0 if none available.
.proc monio_getin
        lda     mon_kbdcnt
        beq     getin1
        dec     mon_kbdcnt
        lda     mon_kbdbuf
        rts
.endproc

.proc getin1
        jsr     _usb_getc_nb    ; non-blocking read
        bcc     @none           ; carry clear = nothing
        cmp     #0
        beq     @none
        cmp     #27             ; ESC?
        beq     @stop
        cmp     #3              ; CTRL-C?
        beq     @stop
        jmp     mapchr
@stop:
        lda     #$7F
        sta     mon_stopflag
@none:
        rts

.endproc

; ------------------------------------------------------------------------------
; CHRIN
; ------------------------------------------------------------------------------
; Get next input character.  If no line has been read yet, read a full
; line from the terminal first.
.proc monio_chrin
        txa
        pha
        tya
        pha
        lda     mon_crflag
        bne     @getbuf
        jsr     getlin          ; read input line
        bcc     @nocr           ; no input at all
        lda     #$80
        sta     mon_crflag
@getbuf:
        ldy     mon_csrcol
        cpy     mon_lastcol
        bcs     @eol
        lda     (mon_lineptr),y
        inc     mon_csrcol
        jmp     @out
@eol:
        lda     #0
        sta     mon_crflag
        inc     mon_csrcol
@nocr:
        lda     #13             ; return CR
@out:
        sta     mon_tmpbuf
        pla
        tay
        pla
        tax
        lda     mon_tmpbuf
        clc
        rts

.endproc

; ------------------------------------------------------------------------------
; getlin
; ------------------------------------------------------------------------------
; Read UART input into line buffer until CR.
; Returns carry set if there was input.
.proc getlin
        cli
        jsr     chkcol
        ldy     mon_csrcol
        lda     mon_csrrow
        pha                     ; save current row
        lda     #0
        sta     mon_stopflag
        sta     mon_crflag      ; shares location with ESCFLAG
@wait:
        jsr     _usb_getc       ; blocking wait for character
        bit     mon_crflag
        bmi     @eschdl         ; in ESC sequence
        cmp     #27             ; ESC?
        beq     @escs
        cmp     #8              ; backspace?
        beq     @bs
        cmp     #127            ; delete?
        beq     @bs
        jsr     mapchr
        beq     @wait           ; unmappable => ignore
        cmp     #13             ; CR or LF?
        beq     @done
        cpy     #NUMCOLS        ; line full?
        bcs     @wait
        sta     (mon_lineptr),y
        iny
        jsr     _usb_putc       ; echo
        jmp     @wait

@done:
        ; trim trailing spaces, set LASTCOL
        ldy     #NUMCOLS-1
@trim:
        lda     (mon_lineptr),y
        cmp     #' '
        bne     @found
        dey
        bne     @trim
@found:
        iny
        sty     mon_lastcol
        sty     mon_termcol
        pla                     ; saved row
        cmp     mon_csrrow
        beq     @samerow
        ldy     #0
        sty     mon_csrcol
        sec
        rts
@samerow:
        cpy     mon_csrcol
        beq     @noinput
        bcs     @hasinput
        sty     mon_csrcol
@noinput:
        clc
        rts
@hasinput:
        sec
        rts

; --- backspace ---
@bs:
        cpy     #0
        beq     @wait
        dey
        lda     #8
        jsr     _usb_putc
        lda     #' '
        sta     (mon_lineptr),y
        jsr     _usb_putc
        lda     #8
        jsr     _usb_putc
        jmp     @wait

; --- ESC sequence start ---
@escs:
        lda     #$80
        .byte   $2C             ; BIT abs - skip next 2-byte instruction
@escclr:
        lda     #$00
        sta     mon_crflag
        jmp     @wait

@eschdl:
        bit     mon_crflag
        bvs     @esc3
        ; waiting for second char
        cmp     #'['
        bne     @escclr
        lda     #$C0            ; wait for third char
        sta     mon_crflag
        jmp     @wait

@esc3:
        ; third char of ESC sequence
        cmp     #'A'
        beq     @cup
        cmp     #'B'
        beq     @cdn
        cmp     #'C'
        beq     @crt
        cmp     #'D'
        beq     @clt
        bne     @escclr

@cup:
        lda     mon_csrrow
        cmp     mon_rowlimit
        beq     @escclr
        jsr     rowup
        lda     #'A'
        jmp     @sendesc

@cdn:
        lda     mon_csrrow
        cmp     #NUMROWS-1
        beq     @cdn1
        jsr     rowdn
        lda     #'B'
        jmp     @sendesc
@cdn1:
        jsr     scrl
        lda     #10
        jsr     _usb_putc
        jmp     @escclr

@clt:
        cpy     #0
        beq     @escclr
        dey
        jmp     @sendesc_D

@crt:
        cpy     #NUMCOLS-1
        beq     @escclr
        iny
        lda     #'C'
        jmp     @sendesc

@sendesc_D:
        lda     #'D'
@sendesc:
        pha
        lda     #27
        jsr     _usb_putc
        lda     #'['
        jsr     _usb_putc
        pla
        jsr     _usb_putc
        jmp     @escclr

.endproc

; ------------------------------------------------------------------------------
; mapchr
; ------------------------------------------------------------------------------
; Map input character:
;   - clear bit 7
;   - ignore codes < 32 (except map LF to CR)
;   - ignore LF immediately after CR
;   - return Z set if character should be ignored
.proc mapchr
        pha
        lda     mon_lastrecv
        sta     mon_tmpbuf
        pla
        sta     mon_lastrecv
        and     #$7F
        cmp     #13
        beq     @iscr
        cmp     #10
        bne     @notlf
        lda     #13
        cmp     mon_tmpbuf      ; previous was CR?
        beq     @null           ; ignore LF after CR
        bne     @iscr           ; else treat LF as CR
@notlf:
        cmp     #' '
        bcc     @null           ; control char => ignore
        ora     #0              ; clear Z
        rts
@null:
        lda     #0
        rts
@iscr:
        lda     #13
        rts

.endproc

; ------------------------------------------------------------------------------
; CHROUT
; ------------------------------------------------------------------------------
; Write output character (in A).
.proc monio_chrout
        sta     mon_lastprnt
        tya
        pha
        lda     mon_lastprnt
        cmp     #13
        bne     @notcr
        jsr     preol           ; print remainder of line buffer
        jsr     proccr          ; handle CR/LF
        jmp     @out
@notcr:
        cmp     #$80
        bcs     @out            ; suppress non-printable >= $80
        cmp     #$20
        bcc     @out            ; suppress control codes < $20
        jsr     chkcol
        lda     mon_lastprnt
        jsr     _usb_putc
        ldy     mon_csrcol
        sta     (mon_lineptr),y
        iny
        sty     mon_csrcol
        sty     mon_termcol
        lda     #0
        sta     mon_crflag
        cpy     #NUMCOLS
        bcc     @out
        jsr     proccr
@out:
        pla
        tay
        lda     mon_lastprnt
        clc
        rts

.endproc

; ------------------------------------------------------------------------------
; proccr
; ------------------------------------------------------------------------------
; Process carriage return - output CR/LF, advance line buffer.
.proc proccr
        tya
        pha
        txa
        pha
        lda     #13
        jsr     _usb_putc
        lda     #10
        jsr     _usb_putc
        lda     #0
        sta     mon_termcol
        sta     mon_csrcol
        sta     mon_lastcol
        sta     mon_crflag
        lda     mon_csrrow
        cmp     #NUMROWS-1
        bne     @noscrl
        jsr     scrl
        jmp     @done
@noscrl:
        jsr     rowdn
@done:
        pla
        tax
        pla
        tay
        rts

.endproc

; ------------------------------------------------------------------------------
; rowdn
; ------------------------------------------------------------------------------
.proc rowdn
        inc     mon_csrrow
        clc
        lda     mon_lineptr
        adc     #NUMCOLS
        sta     mon_lineptr
        lda     mon_lineptr+1
        adc     #0
        sta     mon_lineptr+1
        rts

.endproc

; ------------------------------------------------------------------------------
; rowup
; ------------------------------------------------------------------------------
.proc rowup
        dec     mon_csrrow
        sec
        lda     mon_lineptr
        sbc     #NUMCOLS
        sta     mon_lineptr
        lda     mon_lineptr+1
        sbc     #0
        sta     mon_lineptr+1
        rts

.endproc

; ------------------------------------------------------------------------------
; scrl
; ------------------------------------------------------------------------------
; Scroll screen buffer up one line.
.proc scrl
        pha
        txa
        pha
        tya
        pha
        lda     mon_csrrow
        pha
        lda     mon_lineptr
        pha
        lda     mon_lineptr+1
        pha
        lda     #<mon_linebuf
        sta     mon_lineptr
        lda     #>mon_linebuf
        sta     mon_lineptr+1
        ldx     #NUMROWS-1
@row:
        lda     mon_lineptr
        sta     mon_scrollptr
        lda     mon_lineptr+1
        sta     mon_scrollptr+1
        jsr     rowdn
        ldy     #NUMCOLS-1
@col:
        lda     (mon_lineptr),y
        sta     (mon_scrollptr),y
        dey
        bpl     @col
        dex
        bne     @row
        jsr     clrl            ; clear bottom row
        lda     mon_rowlimit
        beq     @skip
        dec     mon_rowlimit
@skip:
        pla
        sta     mon_lineptr+1
        pla
        sta     mon_lineptr
        pla
        sta     mon_csrrow
        pla
        tay
        pla
        tax
        pla
        rts

.endproc

; ------------------------------------------------------------------------------
; clrl
; ------------------------------------------------------------------------------
; Clear current line buffer row (fill with spaces).
.proc clrl
        ldy     #NUMCOLS-1
        lda     #' '
@loop:
        sta     (mon_lineptr),y
        dey
        bpl     @loop
        rts

.endproc

; ------------------------------------------------------------------------------
; clrlb
; ------------------------------------------------------------------------------
; Clear entire line buffer.
.proc clrlb
        lda     #<mon_linebuf
        sta     mon_lineptr
        lda     #>mon_linebuf
        sta     mon_lineptr+1
        lda     #0
        sta     mon_csrrow
        ldx     #NUMROWS
@loop:
        jsr     clrl
        jsr     rowdn
        dex
        bne     @loop
        jsr     rowup           ; went one too far
        rts

.endproc

; ------------------------------------------------------------------------------
; chkcol
; ------------------------------------------------------------------------------
; Make sure terminal cursor agrees with CSRCOL.
.proc chkcol
        ldy     mon_csrcol
        cpy     mon_termcol
        bcs     @fwd
        ; cursor moved back - reprint from start of line
        lda     #13
        jsr     _usb_putc
        ldy     #0
@rpl:
        cpy     mon_csrcol
        beq     @fwd
        lda     (mon_lineptr),y
        jsr     _usb_putc
        iny
        jmp     @rpl
@fwd:
        beq     @done
@fwd2:
        lda     (mon_lineptr),y
        jsr     _usb_putc
        dey
        cpy     mon_termcol
        bne     @fwd2
@done:
        sty     mon_termcol
        rts

.endproc

; ------------------------------------------------------------------------------
; prline
; ------------------------------------------------------------------------------
; Re-print the current line from the beginning.
.proc prline
        lda     #13
        jsr     _usb_putc
        lda     #0
        sta     mon_termcol
        jmp     preol

.endproc

; ------------------------------------------------------------------------------
; preol
; ------------------------------------------------------------------------------
; Print characters in line buffer after terminal cursor position.
.proc preol
        ldy     #NUMCOLS-1
@scan:
        cpy     mon_termcol
        beq     @out
        bcc     @out
        lda     (mon_lineptr),y
        dey
        cmp     #' '
        beq     @scan
        iny
        sty     mon_tmpbuf      ; last non-space column
        ldy     mon_termcol
        dey
@prt:
        iny
        lda     (mon_lineptr),y
        cmp     #$80
        bcs     @sub
        cmp     #$20
        bcs     @ok
@sub:
        lda     #$20
@ok:
        jsr     _usb_putc
        cpy     mon_tmpbuf
        bne     @prt
        sty     mon_termcol
@out:
        rts

.endproc

; ------------------------------------------------------------------------------
; RAW I/O
; ------------------------------------------------------------------------------
; Unbuffered character I/O - used by LOAD and any other code that needs
; direct byte-at-a-time access to the serial link.
.proc monio_rawgetc
        jmp     _usb_getc       ; blocking read -> A
.endproc

.proc monio_rawputc
        jmp     _usb_putc       ; send byte in A
.endproc

; ------------------------------------------------------------------------------
; monio_reset
; ------------------------------------------------------------------------------
; Initialize monio state - called when entering monitor.
.proc monio_reset
        lda     #0
        sta     mon_lastprnt
        sta     mon_lastrecv
        sta     mon_csrcol
        sta     mon_kbdcnt
        lda     #NUMROWS-1
        sta     mon_csrrow
        sta     mon_rowlimit
        lda     #$FF
        sta     mon_stopflag
        jsr     clrlb
        jsr     proccr          ; set cursor to start of line
        rts
.endproc


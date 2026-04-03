; ==============================================================================
; usb.s - FT245RL driver via VIA2
; ==============================================================================
; The WDC 65C02 SXB has an FTDI FT245RL USB-parallel FIFO connected to
; VIA2 (W65C22N at $7FE0).  The VIA's Port A carries the 8-bit data bus
; and four Port B lines handle the handshake signals.
;
; VIA2 Port B bit assignments:
;   bit 0  TXE#   (input)  - low when FT245 transmit FIFO has room
;   bit 1  RXF#   (input)  - low when FT245 has received data
;   bit 2  WR     (output) - active-high write strobe
;   bit 3  RD#    (output) - active-low read strobe
;
; Procedures: _usb_init, _usb_putc, usb_putc_raw, _usb_getc, _usb_getc_nb,
;   _usb_puts, _usb_putsn, _debug_init, _print_hex_byte
; ------------------------------------------------------------------------------

        .setcpu "65C02"

; ------------------------------------------------------------------------------
; Exports
; ------------------------------------------------------------------------------
        .export _usb_init, _usb_putc, usb_putc_raw
        .export _usb_getc, _usb_getc_nb, _usb_puts, _usb_putsn
        .export _debug_init
        .export _print_hex_byte

; ------------------------------------------------------------------------------
; Imports
; ------------------------------------------------------------------------------
        .import VIA2

; ------------------------------------------------------------------------------
; VIA2 register offsets
; ------------------------------------------------------------------------------
VIA2_PB   = VIA2 + $00          ; Port B data
VIA2_PA   = VIA2 + $01          ; Port A data
VIA2_DDRB = VIA2 + $02          ; Port B data direction
VIA2_DDRA = VIA2 + $03          ; Port A data direction

; ------------------------------------------------------------------------------
; FT245 handshake bits on Port B
; ------------------------------------------------------------------------------
FT_TXE  = $01                   ; bit 0 - TXE# (input)
FT_RXF  = $02                   ; bit 1 - RXF# (input)
FT_WR   = $04                   ; bit 2 - WR   (output)
FT_RD   = $08                   ; bit 3 - RD#  (output)

; ------------------------------------------------------------------------------
;                               BSS
; ------------------------------------------------------------------------------
        .segment "BSS"
usb_ready:      .res 1          ; $FF when initialized

; ------------------------------------------------------------------------------
;                             ZEROPAGE
; ------------------------------------------------------------------------------
        .segment "ZEROPAGE"
usb_ptr:        .res 2

        .segment "CODE"

; ------------------------------------------------------------------------------
; usb_init
; ------------------------------------------------------------------------------
; Initialise VIA2 for FT245 communication. (From stock ROM at $F9C2)
.proc _usb_init
        lda     #(FT_WR | FT_RD)
        sta     VIA2_PB         ; WR=1 (idle), RD#=1 (deasserted)
        lda     #(FT_WR | FT_RD)
        sta     VIA2_DDRB       ; bits 2-3 output, bits 0-1 input
        stz     VIA2_DDRA       ; Port A all input (tri-state data bus)
        lda     #$FF
        sta     usb_ready
        rts
.endproc

; ------------------------------------------------------------------------------
; _usb_putc - void __fastcall__ usb_putc(unsigned char c)
; ------------------------------------------------------------------------------
; A = character. Translates LF ($0A) to CR+LF for terminal output.
; ------------------------------------------------------------------------------
.proc _usb_putc
        cmp     #$0A
        bne     @raw
        pha
        lda     #$0D
        jsr     usb_putc_raw
        pla
@raw:   jmp     usb_putc_raw
.endproc

; ------------------------------------------------------------------------------
; usb_putc_raw - send raw byte (no LF translation)
; ------------------------------------------------------------------------------
; A = byte. Used by monitor for terminal protocol management.
; Preserves A.
; ------------------------------------------------------------------------------
.proc usb_putc_raw
        pha
        phx
        stz     VIA2_DDRA       ; tri-state data bus
        sta     VIA2_PA         ; latch data into VIA Port A register
@wait:
        lda     #FT_TXE
        bit     VIA2_PB         ; test TXE#
        beq     @ready          ; TXE# low = FIFO has room
        dex
        bne     @wait
        plx                     ; timeout - drop byte and return
        pla
        rts
@ready:
        lda     #FT_WR
        tsb     VIA2_PB         ; WR high
        lda     #$FF
        sta     VIA2_DDRA       ; drive data bus
        nop                     ; setup time
        nop
        lda     #FT_WR
        trb     VIA2_PB         ; WR low - FT245 latches byte
        stz     VIA2_DDRA       ; tri-state data bus
        plx
        pla
        rts
.endproc

; ------------------------------------------------------------------------------
; usb_getc
; ------------------------------------------------------------------------------
; Blocking read - waits for data then returns byte in A.
.proc _usb_getc
        stz     VIA2_DDRA       ; data bus input
@wait:
        lda     #FT_RXF
        bit     VIA2_PB         ; test RXF#
        bne     @wait           ; loop while RXF# high (no data)
        lda     #FT_RD
        trb     VIA2_PB         ; RD# low - FT245 drives data bus
        nop                     ; data setup time
        nop
        lda     VIA2_PA         ; read byte
        pha
        lda     #FT_RD
        tsb     VIA2_PB         ; RD# high - release bus
        pla
        rts
.endproc

; ------------------------------------------------------------------------------
; usb_getc_nb
; ------------------------------------------------------------------------------
; Non-blocking read. Returns unsigned int: X:A.
; X=1, A=byte when data available; X=0, A=0 when empty.
; Carry set if a byte was read, clear if nothing available.
.proc _usb_getc_nb
        stz     VIA2_DDRA       ; data bus input
        lda     #FT_RXF
        bit     VIA2_PB         ; test RXF#
        bne     @empty          ; high = no data
        lda     #FT_RD
        trb     VIA2_PB         ; RD# low
        nop
        nop
        lda     VIA2_PA         ; read byte
        pha
        lda     #FT_RD
        tsb     VIA2_PB         ; RD# high
        pla
        ldx     #1              ; high byte = 1 (data available)
        sec
        rts
@empty:
        lda     #0
        ldx     #0              ; high byte = 0 (no data)
        clc
        rts
.endproc

; ------------------------------------------------------------------------------
; usb_puts
; ------------------------------------------------------------------------------
; Print a null-terminated string.
; Pointer: A = low byte, X = high byte.
; Clobbers A, X, Y.
.proc _usb_puts
        sta     usb_ptr
        stx     usb_ptr+1
        ldy     #0
@loop:
        lda     (usb_ptr),y
        beq     @done
        jsr     _usb_putc
        iny
        bne     @loop
        inc     usb_ptr+1       ; handle strings > 255 bytes
        bra     @loop
@done:
        rts
.endproc

; ------------------------------------------------------------------------------
; _usb_putsn - void __fastcall__ usb_putsn(const char *s)
; ------------------------------------------------------------------------------
; A/X = pointer to null-terminated string.
; Sends the string followed by a newline (LF -> CR+LF).
; ------------------------------------------------------------------------------
.proc _usb_putsn
        jsr     _usb_puts
        lda     #$0A
        jmp     _usb_putc
.endproc

; ------------------------------------------------------------------------------
; _print_hex_byte - print byte in A as 2 hex digits via usb_putc_raw
; ------------------------------------------------------------------------------
; Clobbers A.
.proc _print_hex_byte
        pha
        lsr
        lsr
        lsr
        lsr
        jsr     nibble
        pla
        and     #$0F
nibble:
        cmp     #$0A
        bcc     digit
        adc     #$06            ; 'A'-'0'-10-1 (C is set)
digit:
        adc     #$30            ; '0'
        jmp     usb_putc_raw
.endproc

; ------------------------------------------------------------------------------
; _debug_init - void debug_init(void)
; ------------------------------------------------------------------------------
; Calls _usb_init if not already initialized.
; ------------------------------------------------------------------------------
.proc _debug_init
        lda     usb_ready
        bne     @done
        jsr     _usb_init
@done:
        rts
.endproc

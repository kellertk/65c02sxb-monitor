; ==============================================================================
; usb.s - FT245RL driver via VIA2
; ==============================================================================
; The WDC 65C02 SXB has an FTDI FT245RL USB-parallel FIFO connected to
; VIA2 (W65C22S at $7FE0).  The VIA's Port A carries the 8-bit data bus
; and four Port B lines handle the handshake signals.
;
; VIA2 Port B bit assignments:
;   bit 0  TXE#   (input)  - low when FT245 transmit FIFO has room
;   bit 1  RXF#   (input)  - low when FT245 has received data
;   bit 2  WR     (output) - active-high write strobe
;   bit 3  RD#    (output) - active-low read strobe
; ------------------------------------------------------------------------------

        .setcpu "65C02"

; ------------------------------------------------------------------------------
; Exports
; ------------------------------------------------------------------------------
        .export _usb_init, _usb_putc, _usb_getc, _usb_getc_nb, _usb_puts
        .export _print_hex_byte

; ------------------------------------------------------------------------------
; VIA2 register addresses
; ------------------------------------------------------------------------------
VIA2_PB   = $7FE0               ; Port B data
VIA2_PA   = $7FE1               ; Port A data
VIA2_DDRB = $7FE2               ; Port B data direction
VIA2_DDRA = $7FE3               ; Port A data direction

; ------------------------------------------------------------------------------
; FT245 handshake bits on Port B
; ------------------------------------------------------------------------------
FT_TXE  = $01                   ; bit 0 - TXE# (input)
FT_RXF  = $02                   ; bit 1 - RXF# (input)
FT_WR   = $04                   ; bit 2 - WR   (output)
FT_RD   = $08                   ; bit 3 - RD#  (output)

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
        rts
.endproc

; ------------------------------------------------------------------------------
; usb_putc
; ------------------------------------------------------------------------------
; Send the byte in A to the FT245 transmit FIFO.
; Preserves A.
.proc _usb_putc
        pha
        stz     VIA2_DDRA       ; tri-state data bus
        sta     VIA2_PA         ; latch data into VIA Port A register
@wait:
        lda     #FT_TXE
        bit     VIA2_PB         ; test TXE#
        bne     @wait           ; loop while TXE# high (FIFO full)
        lda     #FT_WR
        tsb     VIA2_PB         ; WR high
        lda     #$FF
        sta     VIA2_DDRA       ; drive data bus
        nop                     ; setup time
        nop
        lda     #FT_WR
        trb     VIA2_PB         ; WR low - FT245 latches byte
        stz     VIA2_DDRA       ; tri-state data bus
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
; Non-blocking read - returns byte in A if available, else A=0.
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
        sec                     ; carry set = got data
        rts
@empty:
        lda     #0
        clc                     ; carry clear = nothing
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
; _print_hex_byte - print byte in A as 2 hex digits via _usb_putc
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
        jmp     _usb_putc
.endproc

; ==============================================================================
; init.s - startup segment, initialization code
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Exports
; ------------------------------------------------------------------------------
.export _init, _exit
.export __STARTUP__: absolute = 1

; ------------------------------------------------------------------------------
; Imports
; ------------------------------------------------------------------------------
.import _main
.import _boot_animation
.import _usb_init, _usb_puts, _usb_putc, _print_hex_byte
.import __RAM_START__, __RAM_SIZE__
.import __BSS_RUN__, __BSS_SIZE__
.import copydata, zerobss, initlib, donelib
.include "zeropage.inc"

.segment "RODATA"
banner: .byte $0D, $0A, "WDC 65C02SXB READY", $0D, $0A
        .byte "RAM: ", $00
banner2:.byte " - ", $00

.segment "STARTUP"
_init:
    ldx #$FF
    txs                         ; Initialize stack pointer
    jsr _boot_animation         ; Run boot LED animation
    jsr _usb_init               ; Initialize FT245 USB interface
    lda #<banner
    ldx #>banner
    jsr _usb_puts               ; Print boot banner
    ; Print RAM base (BSS end)
    lda #>(__BSS_RUN__ + __BSS_SIZE__)
    jsr _print_hex_byte
    lda #<(__BSS_RUN__ + __BSS_SIZE__)
    jsr _print_hex_byte
    lda #<banner2
    ldx #>banner2
    jsr _usb_puts               ; Print " - "
    ; Print RAM top
    lda #>(__RAM_START__ + __RAM_SIZE__ - 1)
    jsr _print_hex_byte
    lda #<(__RAM_START__ + __RAM_SIZE__ - 1)
    jsr _print_hex_byte
    lda #$0D
    jsr _usb_putc
    lda #$0A
    jsr _usb_putc
    lda #<(__RAM_START__ + __RAM_SIZE__)
    sta c_sp                    ; Initialize C stack low byte
    lda #>(__RAM_START__ + __RAM_SIZE__)
    sta c_sp + 1                ; Initialize C stack high byte
    jsr zerobss                 ; Zero BSS segment
    jsr copydata                ; Initialize DATA segment
    jsr initlib                 ; Initialize C library
    jsr _main                   ; Call main()

_exit:
    jsr donelib                 ; Run destructors
    brk

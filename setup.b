; P500 burnin-test
; setup file at $d000 in bank 0 and 15
; disassembled by Vossi 02/2019
!cpu 6502
!to "setup.prg", cbm 

*= $d000
	lda #$04			; $d001 will be set from basic loader
	sta $00				; set code bank
	nop
	nop
	nop
	nop
	jmp $2000			; jump to main code

!byte $aa, $aa, $aa, $aa

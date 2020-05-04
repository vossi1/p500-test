; P500 diagnostic-test
; disassembled by Vossi 04/2020
; prepared for ACME reassembling
; fix01 - colorbug on diag-screen
; fix02 - ram search bug BMI skips also bank if CMP has negative check result in sub
; fix03 - search and show all ram banks bur test only 4
; fix04 - bank 0 fault not shown, bank 0 bit direction wrong
; fix05 - graphics char for ROM, (improved) ;)
; fix06 - use all three SID voices
; fix07 - test bank counter
; fix08	- cycles counter line1 for 8 digits space
!cpu 6502
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
; switches
;ROM = 0		; assemble extension rom
!ifdef 	ROM{!to "p2-p.bin", plain
} else{ 	!to "p2-p.prg", cbm }
; ***************************************** CONSTANTS *********************************************
FILL					= $aa		; fills free memory areas with $aa
SYSTEMBANK				= $0f		; systembank
REVSPACE				= $a0		; reverse space for faulty chip
BLACK					= $00		; color codes
WHITE					= $01
CYAN					= $03
YELLOW					= $07
ORANGE 					= $08
LIGHTRED				= $0a
GRAY1					= $0b
GRAY2					= $0c
LIGHTGREEN				= $0d
; ***************************************** ADDRESSES *********************************************
!addr CodeBank			= $00		; code bank register
!addr IndirectBank		= $01		; indirect bank register
!addr ScreenRAM			= $d000		; Screen RAM
!addr VIC				= $d800		; VIC register
; ***************************************** ZERO PAGE *********************************************
!addr pointer1			= $10		; 16bit pointer
!addr ext_color			= $1b		; exterior color
!addr delaycounter		= $1c		; 8bit counter for delay loop
!addr cycles			= $21		; cycles-$24 = cycles counter decimal for 8 digits
!addr test_pages		= $25		; pages to test
!addr temp2				= $26		; temp variable
!addr faulty_bits		= $27		; faulty test bits
!addr storage1			= $28		; temp storage
!addr storage2			= $29		; temp storage
!addr copy_target_bank	= $2c		; copy target bank
!addr copy_target		= $2d		; 16bit copy target address
!addr last_rambank		= $30		; last RAM bank
!addr copy_source_bank	= $32		; copy source bank
!addr copy_source		= $33		; 16bit copy source address
!addr counter			= $35		; counter
!addr color_pointer		= $36		; 16bit colorpointer
!addr test_mask			= $3a		; test		
!addr start_high		= $41		; test start address highbyte	
!addr start_low			= $42		; test start address lowbyte	
!addr start_high		= $41		; test start address highbyte	
!addr temp1				= $43		; temp variable
!addr temp3				= $44		; temp variable
!addr temp4				= $45		; temp variable
!addr check				= $46		; check variable
!addr error				= $47		; error state
!addr temp5				= $49		; temp variable
!addr temp_bank			= $4b		; temp bank variable
!addr pointer2			= $4e		; 16bit pointer
!addr pointer3			= $50		; 16bit pointer
!addr sid_pointer		= $52		; SID register table
!addr tpi1_pointer		= $52		; TPI1 register table - unused -
!addr tpi2_pointer		= $62		; TPI2 register table - unused -
!addr cia_pointer		= $72		; CIA register table - unused -
!addr acia_pointer		= $92		; ACIA register table - unused -
; ****************************************** MACROS ***********************************************
!macro WriteSID .r{			; *** set VDP Register
		sta(sid_pointer + 2*.r),y	; writes data in A to SID via pointer table
}
; ***************************************** ZONE MAIN *********************************************
!zone main
!initmem FILL
*= $2000
	jmp start							; jump to start
	jmp start							; jump to start
	!byte $c3,$c2,$cd,"2"				; cbm-rom ident-bytes 'C'= with init, 'BM', '2' = 4k-block 2
start:	sei								; disable interrupts
		cld								; clear decimal flag
		ldx #$ff
		txs								; reset stack pointer
; init
		ldy #$02						; clear zero page
		lda #$00
clrzplp:sta $0000,y
		iny
		bne clrzplp
; draw screen
		jsr ClearScreen					; clear screen and select graphics character set
		lda #$30						; source = $3000
		sta copy_source+1
		lda #$00
		sta copy_source
		lda #>ScreenRAM					; target = $d000
		sta copy_target+1
		lda #<ScreenRAM
		sta copy_target
		lda CodeBank					; source = active code bank
		sta copy_source_bank
		lda #SYSTEMBANK					; target bank 15
		sta copy_target_bank
		ldx #$04						; 4 pages to copy
		jsr CopyMemory
; color screen
		ldx #SYSTEMBANK					; set indirect bank to 15
		stx IndirectBank
		lda #BLACK						; set background color
		ldy #$00
		ldx #>VIC
		stx color_pointer+1
		ldx #$21						; VIC register #21
		stx color_pointer
		sta (color_pointer),y
		lda #GRAY1						; code will be increased to GRAY2 in sub
		sta ext_color
		jsr SetExteriorColor			; sub: set exterior color VIC register #20
		lda #GRAY2
		ldx #$04						; 4 pages to fill
		stx counter
		ldx #$d4						; colorpointer to $d400 = start color RAM
		stx color_pointer+1
		ldx #$00
		stx color_pointer
		ldy #$00						; start with full page		original $ff ********* PATCHED *********
		jsr FillColor					; sub: fill memory - complete color RAM with light grey
		ldx #$01						; <1 page to fill
		stx counter
		ldx #$d4						; colorpointer to $d40b in color RAM
		stx color_pointer+1
		ldx #$0b
		stx color_pointer
		lda #ORANGE
		ldy #$11						; 17 bytes to fill
		jsr FillColor					; sub: fill memory - color line 0 column 12-28 = orange
		lda #YELLOW
		ldy #$00						; byte 0
		ldx #$d4						; colorpointer to $d40b in color RAM
		stx color_pointer+1
		ldx #$0b
		stx color_pointer
		sta (color_pointer),y			; store yellow to line 0, column 11
		ldy #$12
		sta (color_pointer),y			; store yellow to line 0, column 29
		ldy #$13						; 19 bytes
		ldx #$32						; colorpointer to $d432
		stx color_pointer
		inc counter						; <1 page to fill
		jsr FillColor					; sub: fill memory - color line 1 column 11-29 = yellow
		dec color_pointer+1				; reset colorpointer highbyte to $d4
		lda #WHITE
		inc counter						; <1 page to fill
		ldx #$60						; colorpointer to $d460
		stx color_pointer
		ldy #$08						; 8 bytes
		jsr FillColor					; sub: fill memory - color line 2 column 17-25 = white
		lda #CYAN
		ldy #$1d						; 29 bytes
		inc counter						; <1 page to fill
		ldx #$d5						; colorpointer to $d520
		stx color_pointer+1
		ldx #$20
		stx color_pointer
		jsr FillColor					; sub: fill memory - color line 7 column 1-29
		inc counter
		dec color_pointer+1				; colorpointer to $d5c0
		ldx #$c0
		stx color_pointer
		ldy #$1d
		jsr FillColor					; sub: fill memory - color line 11 column 1-29
		inc counter						; <1 page to fill
		ldx #$d6						; colorpointer to $d660
		stx color_pointer+1
		ldx #$60
		stx color_pointer
		ldy #$1d
		jsr FillColor					; sub: fill memory - color line 15 column 1-29
		inc counter						; <1 page to fill
		ldx #$d7						; colorpointer to $d700
		stx color_pointer+1
		ldx #$00
		stx color_pointer
		ldy #$1d
		jsr FillColor					; sub: fill memory - color line 19 column 1-29
		inc counter						; <1 page to fill
		dec color_pointer+1				; colorpointer to $d7a0
		ldx #$a0
		stx color_pointer
		ldy #$1d
		jsr FillColor					; sub: fill memory - color line 23 column 1-29
		ldx #$00
; search RAM banks
findram:stx IndirectBank				; switch to indirect bank 0
		lda #$08						; pointer1 to $0800
		sta pointer1+1
		lda #$00
		sta pointer1
		jsr SearchRAM					; sub: search for RAM - Z=1 RAM found
		bmi noram						; skip if no RAM found
		inc last_rambank				; increase RAM bank present counter
noram:	inx								; increase bank
		cpx #$0f						; check if last possible RAM bank	orig. $04 ********* PATCHED *********
		bne findram						; search next RAM bank
		lda last_rambank				; load RAM banks 		
; write found banks to screen
		jsr Hex2Screencode				; sub: calc screencode digits for byte in A to AY
		lda #$09						; pointer to $d009
		sta pointer1
		lda #$d0
		sta pointer1+1
		tya								; move lower digit to A
		ldy #SYSTEMBANK
		sty IndirectBank				; switch to bank 15
		ldy #$00
		sta (pointer1),y				; write RAM banks count to screen line 0 column 9
		lda pointer1+1
		clc
		adc #$04						; add $04 to highbyte pointer1 = color RAM
		sta pointer1+1
		lda #LIGHTGREEN
		sta (pointer1),y				; store color for banks found
		ldy #$28						; add 40 for next line			********* PATCHED *********
		lda #LIGHTRED
		sta (pointer1),y				; store color for testbank
		dec last_rambank				; decrease bank count to get last bank (first bank = 0)
		lda last_rambank				; check if more than 4 RAM banks ********* PATCHED *********
		cmp #$04
		bmi max4bnk						; skip if last bank is <= 3
		lda #$03						; reduce last bank to test = 3
		sta last_rambank
max4bnk:jmp Main						; jump to main code
; ----------------------------------------------------------------------------
; search for RAM - returns Z=1 if RAM found in page at pointer1
SearchRAM:
		clv								; why ?
		ldy #$00						; clear counter Y
		lda #$a5						; value = $a5
framsto:sta (pointer1),y				; store to RAM				
		iny
		bne framsto						; next byte
framchk	lda (pointer1),y				; load from RAM
		cmp #$a5						; check value
		beq framfnd						; end check if RAM found N=0
		iny
		bne framchk						; next byte if not equal
		dey								; Y=$ff -> N=1 / no RAM found
framfnd	rts								; N=0 RAM found
; ----------------------------------------------------------------------------
; Clear screen and select graphics character set
ClearScreen:
		lda #>VIC						; pointer = $d800 VIC  ********* PATCHED *********
		sta pointer1+1
		ldy #<VIC
		sty pointer1
		lda #SYSTEMBANK
		sta IndirectBank				; switch to bank 15
		ldy #$18						; VIC memory pointers register
		lda #$41						; clear bit1 CB11 = graphics character set
		sta (pointer1),y				; store to screen RAM
		lda #>ScreenRAM					; pointer to screen RAM
		sta pointer1+1
		lda #$20						; fill with <SPACE>
		ldx #$04						; 4 pages to clear
clrscr: sta (pointer1),y				; store to screen RAM
		iny
		bne clrscr						; next byte
		inc pointer1+1
		dex
		bne clrscr						; next page
		rts
; ----------------------------------------------------------------------------
; Fills color memory Y bytes, counter pages with A
FillColor:
		sta (color_pointer),y			; store in indirect bank
		dey
		bne FillColor					; next byte
		inc color_pointer+1				; removed wrong dey ********* PATCHED *********
		dec counter
		bne FillColor					; next page
		rts
; ----------------------------------------------------------------------------
; Main loop
Main:	jsr CopySIDTable				; init zeropage $52-$8c from SIDTable
		lda #SYSTEMBANK
		sta IndirectBank				; switch to bank 15
		jsr PlaySound					; play sound
		jsr SetExteriorColor			; increase exterior color after each cycle
		jsr DummySub					; Call 19x dummy-subroutine
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr DummySub
		jsr Test						; sub: test, copy code, switch to new bank
; increase cycles counter
		ldx #$03						; 3 digits for cycles
		sed								; switch do decimal mode
		sec								; set carry to add 1 cycle
mnxtdig:lda cycles,x					; start with last digit
		adc #$00						; use A with ADC instead of INX/INY because indexed access
		sta cycles,x					; increase last digit $23... -> $21 if 0
		bcc mdign00						; skip if not 00
		dex
		bpl mnxtdig						; next digit if 00
; write cycles to screen
mdign00:lda #$20 + 40					; print cycles in line 1 
		sta pointer1
		lda #$d0
		sta pointer1+1					; set screen pointer to cycles counter
		lda #SYSTEMBANK
		sta IndirectBank				; switch to systembank
		ldx #$ff
		ldy #$00
mskpzer:inx
		cpx #$04
		bne mnotlst						; check digit if not last digit
		dex								; if last digit was zero, decrease X to print 0
		bne mprtdig
mnotlst:lda cycles,x
		beq mskpzer
		and #$f0						; clear lower nibble
		beq mprtdig						; print if < 10
mprtnxt:jsr UpperNibble2Screencode		; sub: convert upper nibble to digit screencode
		sta (pointer1),y				; print upper nibble
		iny
mprtdig:lda cycles,x					; load digit again
		jsr Nibble2Screencode			; sub: convert nibble to digit screencode
		sta (pointer1),y				; write digit to screen
		iny								; increase screen pointer to next place
		inx								; increase digit counter
		lda cycles,x					; load next digit
		cpx #$04
		bne mprtnxt						; print next digit
		cld								; clear decimal flag
		jmp Main						; next test cycle
; ----------------------------------------------------------------------------
; Dummy subroutine
DummySub:
		rts
		rti
; ----------------------------------------------------------------------------
; play sound
PlaySound:	
		ldy #$00						; clear Y for indirect writes
		lda IndirectBank
		sta temp_bank							; remember target bank
		lda #SYSTEMBANK
		sta IndirectBank				; indirect bank = bank 15
		+WriteSID $18					; volume = 15 (A already 15)
		lda #$1a
		+WriteSID $0c					; voice 2 AD
		lda #$0a
		+WriteSID $0d					; voice 2 SR
		lda #$4e						; frequency = 200000 ~ note D#6
		+WriteSID $08					; voice 2 frequency hi
		lda #$20
		+WriteSID $07					; voice 2 frequency low
		lda #$18
		+WriteSID $13					; voice 3 AD	with 3 voices ********* PATCHED *********
		lda #$0a
		+WriteSID $14					; voice 3 SR
		lda #$09						; frequency = 2500
		+WriteSID $0f					; voice 3 frequency hi
		lda #$c4
		+WriteSID $0e					; voice 3 frequency low
		lda #$07						; frequency = 2000 ~ note B2
		+WriteSID $01					; voice 1 frequency hi
		lda #$d0
		+WriteSID $00					; voice 1 frequency low
		lda #$15
		+WriteSID $0b					; voice 2 triangle,ringmod,gate
		lda #$21
		+WriteSID $12					; voice 3 triangle, gate
		lda #$14
		+WriteSID $0b					; voice 2 triangle, ringmod
		lda #$20
		+WriteSID $12					; voice 3 triangle
		lda temp_bank
		sta IndirectBank				; restore target bank
		rts
; ----------------------------------------------------------------------------
; test, copy code, switch to new bank
Test:
		lda #$ff
		sta test_mask					; test-mask - $ff = test all bits
		ldy last_rambank
		sty $4a							; store last bank to $4a
		ldx CodeBank
		stx copy_source_bank			; source bank = actual codebank
		dex								; decrease bank
		bpl notbnk0						; skip if codebank is > bank 0
		ldx last_rambank				; load last bank if code is in bank 0
notbnk0:stx copy_target_bank			; starget bank = bank below code or last if code is in bank 0
		stx $31							; store target bank to $31
		ldx copy_target_bank
		stx IndirectBank				; set indirect bank = target bank
		jsr RAMTest						; sub: RAM Test
		ldx copy_target_bank
		stx copy_source_bank
		dex
		bpl l2247
		ldx last_rambank
l2247:	stx copy_target_bank
		dec $4a
		bne notbnk0
		ldy last_rambank
		sty $4a
		ldx CodeBank
		dex
		bpl l2258
		ldx last_rambank
l2258:	lda $15,x
		beq l2267
l225c:	dex
		bpl l2261
		ldx last_rambank
l2261:	dec $4a
		bne l2258
		beq l2295
l2267:	stx temp5
		txa
		ldx #$00
		ldy #$00
		jsr l26a2
		lda CodeBank
		jsr l2699
		ldx #$29
		inx
		jsr CopyMemory
		beq l2282
		ldx temp5
		bpl l225c
l2282:	ldy CodeBank
		ldx temp5
		stx CodeBank
		nop
		nop
		nop
		nop
		sty copy_target_bank
		ldx copy_target_bank
		stx IndirectBank
		jsr RAMTest
l2295:	jsr l2299
		rts
; ----------------------------------------------------------------------------
l2299:	lda #SYSTEMBANK
		sta IndirectBank
		ldy #$02
		ldx #$00
		lda #$08
		jsr l22e8
		lda CodeBank
		ldx #$d0
		ldy #$00
		jsr l26a2
		lda #$0f
		jsr l2699
		ldx #$08
		jsr CopyMemory
		lda #$d4
		ldy #$00
		ldx #$d0
		jsr l22e8
		lda #$0f
		sta test_mask
		lda #$d8
		ldy #$00
		ldx #$d4
		jsr l22e8
		lda #$0f
		ldx #$d0
		ldy #$00
		jsr l26a2
		lda CodeBank
		jsr l2699
		ldx #$08
		jsr CopyMemory
		rts
; ----------------------------------------------------------------------------
; RAM test
RAMTest:	
		lda #$00						; test start address = $0002
		tax
		ldy #$02						; test start address 
l22e8:	sty start_low					; remember start address lowbyte (start with $02)
		stx start_high					; remember start address highbyte
		sta test_pages					; $00 = test all pages
		dey
		sty temp2						; remember last test page for downwards test end check
		lda #$00
		sta pointer1					; pointer1 lowbyte = $00
		sta pointer3
		lda #>ScreenRAM
		sta pointer3+1					; pointer3 = screen RAM
		lda IndirectBank
		sta temp_bank					; remeber sctual test bank	********* PATCHED *********
		ldy #SYSTEMBANK					;							********* PATCHED *********
		sty IndirectBank				; switch to bank 15			********* PATCHED *********
		jsr Nibble2Screencode			; convert bank number to screen code
		ldy #(40*1 + 9)					; Y = line 2 char 6			********* PATCHED *********
		sta (pointer3),y				; write actual test bank number to screen
		lda temp_bank					;							********* PATCHED *********
		sta IndirectBank				; restore test bank			********* PATCHED *********
		cmp #$0f						; check if target = bank 15
		beq notbnkf						; skip if not bank 15
		jsr PlaySound					; play sound
notbnkf:ldy start_low					; start with Y = $02
		lda start_high
		sta pointer1+1					; start with page $00
; test1 with upcounting value
test1lp:tya								; Y as test-byte
		sta temp3						; remember test-byte
		sta (pointer1),y				; store test value to test-bank
		lda (pointer1),y
		eor temp3						; check test-byte
		and test_mask					; mask out bits to test - standard $ff = all tested
		beq +
		jsr TestError					; jump to test error
+		iny
		bne test1lp						; next byte
		inc pointer1+1					; increase highbyte
		lda pointer1+1
		cmp test_pages					; check if last test page
		bne test1lp						; next page
		jsr ResetStartAddress			; reset start address for next test
; test 2 with address highbyte
test2lp:tya
		sta temp3
		lda (pointer1),y				; check byte from last test again
		eor temp3
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		lda pointer1+1
		sta (pointer1),y
		lda (pointer1),y
		eor pointer1+1					; check with address highbyte
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		iny
		bne test2lp
		inc pointer1+1
		lda pointer1+1
		cmp test_pages
		bne test2lp
		jsr PlaySound					; play sound
		jsr ResetStartAddress			; reset start address for next test
; test 3 first byte with $55, second with $aa
		lda #$55
		sta temp3
		lda #$aa
		sta temp4
test3lp:lda (pointer1),y				; check byte from last test again
		eor pointer1+1
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		lda #$55
		sta (pointer1),y
		lda (pointer1),y
		eor temp3						; chcek with $55
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		iny								; next byte
		lda (pointer1),y				; check byte from last test again
		eor pointer1+1
		and test_mask
		beq +
		jsr TestError					; jump to test error
+ 		lda #$aa
		sta (pointer1),y
		lda (pointer1),y
		eor temp4						; check second byte with $aa
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		iny
		bne test3lp
		inc pointer1+1
		lda pointer1+1
		cmp test_pages
		bne test3lp
		jsr ResetStartAddress			; reset start address for next test
; test 4 first byte with $aa, second with $55
test4lp:lda (pointer1),y				; check byte from last test again
		eor temp3
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		lda #$aa
		sta (pointer1),y
		lda (pointer1),y
		eor temp4						; check with $aa
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		iny								; next byte
		lda (pointer1),y				; check byte from last test again
		eor temp4
		and test_mask
		beq l23c1
		jsr TestError					; jump to test error
l23c1:	lda #$55
		sta (pointer1),y
		lda (pointer1),y
		eor temp3						; check with $55
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		iny
		bne test4lp
		inc pointer1+1
		lda pointer1+1
		cmp test_pages
		bne test4lp
		jsr PlaySound					; play sound
		jsr ResetStartAddress			; reset start address for next test
; test 5 test with $5a
		ldx #$5a
		stx check
test5lp:lda (pointer1),y				; check byte from last test again
		eor temp4
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		txa
		sta (pointer1),y
		lda (pointer1),y
		eor check						; check with $5a
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		iny								; next byte
		lda (pointer1),y				; check byte from last test again
		eor temp3
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		txa
		sta (pointer1),y
		lda (pointer1),y
		eor check						; check with $5a
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		iny
		bne test5lp
		inc pointer1+1
		lda pointer1+1
		cmp test_pages
		bne test5lp
		jsr MaxStartAddress				; set address to maximum
; test 6 with $a5 downwards
		ldx #$5a
		stx temp3
		ldx #$a5
		stx temp4
test6lp:lda (pointer1),y				; check byte from last test again
		eor temp3
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		txa
		sta (pointer1),y
		lda (pointer1),y
		eor temp4						; check with $a5
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		dey								; next byte down
		cpy #$ff
		bne test6lp
		dec pointer1+1
		lda pointer1+1
		cmp start_high
		bne test6lp						; next page down
; check last page because downwards
tst6alp:lda (pointer1),y				; check byte from last test again
		eor temp3
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		txa
		sta (pointer1),y
		lda (pointer1),y
		eor temp4						; check with $a5
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		dey
		cpy temp2
		bne tst6alp						; next byte down
		jsr PlaySound					; play sound
		jsr MaxStartAddress				; set address to maximum
; test 7 with $5a downwards
		ldx #$5a
test7lp:lda (pointer1),y				; check byte from last test again
		eor temp4
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		txa
		sta (pointer1),y
		lda (pointer1),y
		eor temp3						; check with $5a
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		dey
		cpy #$ff
		bne test7lp						; next byte down		
		dec pointer1+1
		lda pointer1+1
		cmp start_high
		bne test7lp						; next page down
; check last page because downwards
tst7alp:lda (pointer1),y				; check byte from last test again
		eor temp4
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		txa
		sta (pointer1),y
		lda (pointer1),y
		eor temp3						; check with $5a
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		dey
		cpy temp2
		bne tst7alp
		jsr ResetStartAddress			; reset start address for next test
; test 8 with $ff
		ldx #$ff
		stx check
test8lp:lda (pointer1),y				; check byte from last test again
		eor temp3
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		txa
		sta (pointer1),y
		lda (pointer1),y
		eor check						; check with $ff
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		iny
		bne test8lp						; next byte
		inc pointer1+1
		lda pointer1+1
		cmp test_pages
		bne test8lp						; next page
		jsr PlaySound					; play sound
		jsr MaxStartAddress				; set address to maximum
; test 9 with $00 downwards
		ldx #$00
		stx temp3
		ldx #$ff
		stx temp4
test9lp:txa
		lda (pointer1),y				; check byte from last test again
		eor temp4
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		sta (pointer1),y
		lda (pointer1),y
		eor temp3						; check with $00
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		dey
		cpy #$ff
		bne test9lp						; next byte down
		dec pointer1+1
		lda pointer1+1
		cmp start_high
		bne test9lp						; next page down
; check last page because downwards
tst9alp:txa
		lda (pointer1),y				; check byte from last test again
		eor temp4
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		sta (pointer1),y
		lda (pointer1),y
		eor temp3						; check with $00
		and test_mask
		beq +
		jsr TestError					; jump to test error
+		dey
		cpy temp2
		bne tst9alp						; next byte down
		rts
; ----------------------------------------------------------------------------
; test error
TestError:
		clv
		sta faulty_bits					; remember faulty bits
		stx storage1					; remember X
		sty storage2					; remember lowbyte
		ldx IndirectBank
		stx temp_bank					; remember defective test bank
		ldy temp_bank					; Y = defective test bank
		cpy #$0f						; check if bank 15
		beq errbnkf						; skip if bank 15
										; removed dey to show bank 0 fault ********* PATCHED *********
		lda #$ff
		sta $0015,y						; store $ff to $15 + defective bank
		ldy temp_bank					; load defective testbank
										; removed dey to show bank 0 fault ********* PATCHED *********
		lda ErrorBarsLow,y				; load screen-pointer to faulty bank from table
		sta pointer2
		lda ErrorBarsHigh,y
		sta pointer2+1
		ldx #$08						; 8 bits to check
		stx temp5
errloop:lda faulty_bits					; load faulty bits - 1=faulty
		clc
		rol								; shift faulty bit in carry
		sta faulty_bits					; store <<faulty bits
		bcc erbitok						; skip if bit not faulty = 0
		jsr ColorFaultyChip				; color faulty RAM chip
erbitok:lda #$03
		clc
		adc pointer2
		sta pointer2					; add 3 to screen pointer for next chip
		bcc +
		inc pointer2+1
+		ldx temp5
		dex								; decrease bit counter
		stx temp5
		bne errloop						; check next bit
l2581:	ldx temp_bank
		stx IndirectBank				; switch back to actual testbank
		ldx storage1					; restore start higbyte
		ldy storage2					; restore lowbyte
		lda #$00
		rts
; ----------------------------------------------------------------------------
errbnkf:lda pointer1+1
		and #$f0
		bne l259f
		lda #$44
		sta pointer2
		lda #$d3
		sta pointer2+1
		jsr ColorFaultyChip
		bvc l2581
l259f:	lda pointer1+1
		cmp #$d4
		bpl l25cd
		lda faulty_bits
		and #$f0
		beq l25b8
		lda #$a4
		sta pointer2
		lda #$d2
		sta pointer2+1
		lda CodeBank
		jsr l25de
l25b8:	lda faulty_bits
		and #$0f
		beq l2581
		lda #$a1
		sta pointer2
		lda #$d2
		sta pointer2+1
		lda CodeBank
		jsr l25de
		bvc l2581
l25cd:	lda #$41
		sta pointer2
		lda #$d3
		sta pointer2+1
		lda CodeBank
		jsr l25de
		bvc l2581
; ----------------------------------------------------------------------------
; color the faulty chip		
ColorFaultyChip:
		lda #SYSTEMBANK
l25de:	sta IndirectBank				; switch to bank 15
		ldy #$00
		lda (pointer2),y				; load char from screen
		bmi coldone						; return if chip already colored
		lda pointer2					
		sta pointer3					; copy screen pointer to pointer 3
		lda pointer2+1
		sta pointer3+1
		ldx #$03						; 3 chars high
colnxty:ldy #$01						; 2 chars wide
colnxtx:lda #REVSPACE
		sta (pointer3),y				; write reverse space to screen
		dey
		bpl colnxtx						; next char at the left
		lda #$04
		clc
		adc pointer3+1					; add $04 to higbyte for color RAM
		sta pointer3+1
		ldy #$01
colcolx:lda #YELLOW
		sta (pointer3),y				; write color
		dey
		bpl colcolx						; next char at the left
		lda pointer3+1
		sec
		sbc #$04						; sub $04 to get back to screen RAM
		sta pointer3+1
		lda #$28
		clc
		adc pointer3					; add 40 for next line
		sta pointer3
		bcc +
		inc pointer3+1
+		dex
		bne colnxty						; next line
coldone:rts
; ----------------------------------------------------------------------------
; Reset RAM test start address
ResetStartAddress:
		ldy start_low
		lda start_high
		sta pointer1+1
		rts
; ----------------------------------------------------------------------------
; Set RAM Test start address to last byte
MaxStartAddress
		ldy test_pages
		dey
		sty pointer1+1
		ldy #$ff
		rts
; ----------------------------------------------------------------------------
; memory copy sub - copies counter pages from source to target
CopyMemory:
		stx counter						; save pages to copy to counter $35, temp $43
		stx temp1
		ldx IndirectBank				; save indirect bank to temp_bank
		stx temp_bank
		ldy #$00						; start low byte Y = $00
		cpy copy_target+1
		bne +							; skip if target higbyte is $00
		ldy #$02						; start low byte = $02 if page 0
+		sty $3f							; save start low byte in $3f
-		ldx copy_source_bank
		stx IndirectBank				; set source bank
		lda (copy_source),y				; load source byte
		ldx copy_target_bank			; set target bank
		stx IndirectBank
		sta (copy_target),y				; save byte to target
		iny
		bne -							; copy next byte
		inc copy_source+1				; increase high bytes
		inc copy_target+1
		dec counter						; decrease page counter
		bne -							; copy next page

		lda temp1						; restore page to counter
		sta counter
		lda #$00						; clear $47
		sta error
		ldy $3f							; load start low byte
		lda temp3							; check if temp4 = $00
		ora temp4
		and temp4
		bne +							; skip if $45 not $00
		ldy #$48						; start at low byte $48
+		lda temp3
		sta copy_source+1				; source high byte = $44
		lda temp4
		sta copy_target+1				; target high byte = $45
-		ldx copy_source_bank			; set source bank
		stx IndirectBank
		lda (copy_source),y				; load source byte
		sta check						; save to check
		ldx copy_target_bank			; set target bank
		stx IndirectBank
		lda (copy_target),y				; load target byte
		eor check						; A=0 if source = target
		ora error						; add state to error variable  
		sta error						; save new state
		iny
		bne -							; check next byte
		inc copy_source+1				; increase high bytes
		inc copy_target+1
		dec counter						; decrease page counter
		bne -							; check next page
		ldx temp_bank							; restore indirect bank
		stx IndirectBank
		lda error						; return error state 0=ok, 1=error
		rts

l2699:	sta copy_source_bank
		stx copy_source+1
		stx temp3
		sty copy_source
		rts
l26a2:	sta copy_target_bank
		stx copy_target+1
		stx temp4
		sty copy_target
		rts
; ----------------------------------------------------------------------------
; Calc screencode digits for byte in A to AY
Hex2Screencode:	
		pha								; remember value on stack
		jsr Nibble2Screencode
		tay								; remember lower digit in Y
		pla								; restore value
UpperNibble2Screencode:
		lsr								; upper nibble -> lower nibble
		lsr
		lsr
		lsr
Nibble2Screencode:
		and #$0f						; clear upper nibble
		cmp #$0a						; compare if < 10
		bmi +							; skip if < 10
		sec
		sbc #$09						; sub 9 -> calc screencode A-F 
		bne ++
+		ora #$30						; add $30 -> screencode 0-9 of the digit
++		rts
; ----------------------------------------------------------------------------
; unused
		jsr +
		asl
		asl
		rts

+		clc
		sta temp1
		asl
		asl
		adc temp1
		rts
; ----------------------------------------------------------------------------
; Set exterior color
SetExteriorColor:
		inc ext_color					; increase exterior color
		lda #>VIC
		sta color_pointer+1
		lda #$20						; VIC register #20
		sta color_pointer
		lda ext_color
		ldy #$00
		sta (color_pointer),y
		rts
; ----------------------------------------------------------------------------
; unused - delay $1000000 loops
		ldy #$ff
		ldx #$ff
-		dey
		bne -
		dex
		bne -
		dec delaycounter
		bne -
		rts
; ----------------------------------------------------------------------------
; unused - copies CIA pointer to ZP
		lda #cia_pointer
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #<CIATable
		ldx #>CIATable
		ldy #$1f
		jsr CopyTable
		rts
; ----------------------------------------------------------------------------
; unused - copies Triport2 pointer to ZP
		lda #tpi2_pointer
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #<TPI2Table
		ldx #>TPI2Table
		ldy #$0f
		jsr CopyTable
		rts
; ----------------------------------------------------------------------------
; unused - copies Triport1 pointer to ZP
		lda #tpi1_pointer
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #<TPI1Table
		ldx #>TPI1Table
		ldy #$0f
		jsr CopyTable
		rts
; ----------------------------------------------------------------------------
; unused - copies ACIA pointer to ZP
		lda #acia_pointer
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #<ACIATable
		ldx #>ACIATable
		ldy #$07
		jsr CopyTable
		rts
; ----------------------------------------------------------------------------
; copy sid-pointer-table to zeropage for indirect access
CopySIDTable:
		lda #sid_pointer				; sid pointer in ZP
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #<SIDTable					; XA = SIDTable
		ldx #>SIDTable
		ldy #$39						; bytes to copy = $00-$39
		jsr CopyTable					; sub: copy table
		rts
; ----------------------------------------------------------------------------
; unused
		lda #$f0
		sta $fffa
		lda #$00
		sta $fffb
		lda #$f2
		sta $fffc
		lda #$00
		sta $fffd
		lda #$f4
		sta $fffe
		lda #$00
		sta $ffff
		lda #$f0
		sta $f0
		sta $f2
		sta $f4
		lda #$21
		sta $f1
		sta $f3
		sta $f5
		rts
; ----------------------------------------------------------------------------
; copy $00-Y bytes in codebank from XA to pointer1
CopyTable:
		sta pointer2					; store XA in pointer2
		stx pointer2+1
		lda CodeBank					; load actual code bank
		sta IndirectBank				; set indirect = code bank
cpytblp:lda (pointer2),y				; copy byte
		sta (pointer1),y
		dey
		bpl cpytblp						; next byte 
		rts
; ************************************* ZONE TABLES ***********************************************
!zone tables
Messages:
		!scr "LO ADR BYTE TEST "
		!scr "HI ADR BYTE TEST "
		!scr "CHKRBRD $55, $AA AA, $55 "
		!scr "MARCH INC ADR $5A "
		!scr "DEC ADR $A5 5A "
		!scr "INC ADR $FF "
		!scr "DEC ADR $00 "
		!scr "STATIC RAM TESTS  "
		!scr "6526 TIMERS TESTS "

ErrorBarsLow:
		!byte $89, $a9, $49, $c1 ; ************ patched last byte to first

ErrorBarsHigh:
		!byte $d2, $d0, $d1, $d1 ; *********** patched last byte to first

;282a
!scr " * *  BAD PROGRAM CHECKSUM  * * "

;284a
; unused - VIC pointer
VICTable:
		!byte $00, $d8, $01, $d8, $02, $d8, $03, $d8
		!byte $04, $d8, $05, $d8, $06, $d8, $07, $d8
		!byte $08, $d8, $09, $d8, $0a, $d8, $0b, $d8
		!byte $0c, $d8, $0d, $d8, $0e, $d8, $0f, $d8
		!byte $10, $d8, $11, $d8, $12, $d8, $13, $d8
		!byte $14, $d8, $15, $d8, $16, $d8, $17, $d8
		!byte $18, $d8, $19, $d8, $1a, $d8, $1b, $d8
		!byte $1c, $d8, $1d, $d8, $1e, $d8, $1f, $d8
		!byte $20, $d8, cycles, $d8, $22, $d8, $23, $d8
		!byte $24, $d8, $25, $d8, $26, $d8, $27, $d8
		!byte $28, $d8, $29, $d8, $2a, $d8, $2b, $d8
		!byte $2c, $d8, $2d, $d8, $2e, $d8
; SID pointer copied to $52
SIDTable:
		!byte $00, $da, $01, $da, $02, $da, $03, $da
		!byte $04, $da, $05, $da, $06, $da, $07, $da
		!byte $08, $da, $09, $da, $0a, $da, $0b, $da
		!byte $0c, $da, $0d, $da, $0e, $da, $0f, $da
		!byte $10, $da, $11, $da, $12, $da, $13, $da
		!byte $14, $da, $15, $da, $16, $da, $17, $da
		!byte $18, $da, $19, $da, $1a, $da, $1b, $da
		!byte $1c, $da
; unused - CIA Table copied to $72
CIATable:
		!byte $00, $dc, $01, $dc, $02, $dc, $03, $dc
		!byte $04, $dc, $05, $dc, $06, $dc, $07, $dc
		!byte $08, $dc, $09, $dc, $0a, $dc, $0b, $dc
		!byte $0c, $dc, $0d, $dc, $0e, $dc, $0f, $dc
; unused - ACIA pointer copied to $92
ACIATable:
		!byte $00, $dd, $01, $dd, $02, $dd, $03, $dd
; unused - Triport1 pointer copied to $52
TPI1Table:
		!byte $00, $de, $01, $de, $02, $de, $03, $de
		!byte $04, $de, $05, $de, $06, $de, $07, $de
; unused - Triport2 pointer copied to $62
TPI2Table:
		!byte $00, $df, $01, $df, $02, $df, $03, $df
		!byte $04, $df, $05, $df, $06, $df, $07, $df
; ************************************* ZONE SCREENDATA *******************************************
!zone screendata
*= $3000
ScreenData:
		!scr "rambanks"									; ********* PATCHED *********
		!byte $20, $20, $20, $65, $10, $05, $14, $20
		!byte $09, $09, $20, $04, $09, $01, $07, $0e
		!byte $0f, $13, $14, $09, $03, $67, $20, $20
		!scr "cycles  "									; ********* PATCHED *********

		!scr "testbank"
		!byte $20, $20, $20, $63, $63, $63, $63, $63
		!byte $63, $63, $63, $63, $63, $63, $63, $63
		!byte $63, $63, $63, $63, $63, $63, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!scr " d7 d6 d"
		!scr "5 d4 d3 "
		!scr "d2 d1 d0"
		!byte $20, $20, $20, $20, $20, $20, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $4f, $50, $20, $4f, $50, $20, $4f
		!byte $50, $20, $4f, $50, $20, $4f, $50, $20
		!byte $4f, $50, $20, $4f, $50, $20, $4f, $50
		!byte $20, $4f, $50, $20, $4f, $50, $20, $20

		!scr "  bank 1"
		!byte $20, $74, $6a, $20, $74, $6a, $20, $74
		!byte $6a, $20, $74, $6a, $20, $74, $6a, $20
		!byte $74, $6a, $20, $74, $6a, $20, $74, $6a
		!byte $20, $74, $6a, $20, $74, $6a, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $4c, $7a, $20, $4c, $7a, $20, $4c
		!byte $7a, $20, $4c, $7a, $20, $4c, $7a, $20
		!byte $4c, $7a, $20, $4c, $7a, $20, $4c, $7a
		!byte $20, $4c, $7a, $20, $4c, $7a, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $33, $33, $20, $33, $34, $20, $33
		!byte $35, $20, $33, $36, $20, $33, $37, $20
		!byte $33, $38, $20, $33, $39, $20, $34, $30
		!byte $20, $20, $20, $20, $20, $20, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $4f, $50, $20, $4f, $50, $20, $4f
		!byte $50, $20, $4f, $50, $20, $4f, $50, $20
		!byte $4f, $50, $20, $4f, $50, $20, $4f, $50
		!byte $20, $4f, $50, $20, $4f, $50, $20, $20

		!scr "  bank 2"
		!byte $20, $74, $6a, $20, $74, $6a, $20, $74
		!byte $6a, $20, $74, $6a, $20, $74, $6a, $20
		!byte $74, $6a, $20, $74, $6a, $20, $74, $6a
		!byte $20, $74, $6a, $20, $74, $6a, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $4c, $7a, $20, $4c, $7a, $20, $4c
		!byte $7a, $20, $4c, $7a, $20, $4c, $7a, $20
		!byte $4c, $7a, $20, $4c, $7a, $20, $4c, $7a
		!byte $20, $4c, $7a, $20, $4c, $7a, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $34, $31, $20, $34, $32, $20, $34
		!byte $33, $20, $34, $34, $20, $34, $35, $20
		!byte $34, $36, $20, $34, $37, $20, $34, $38
		!byte $20, $20, $20, $20, $20, $20, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $4f, $50, $20, $4f, $50, $20, $4f
		!byte $50, $20, $4f, $50, $20, $4f, $50, $20
		!byte $4f, $50, $20, $4f, $50, $20, $4f, $50
		!byte $20, $4f, $50, $20, $4f, $50, $20, $20

		!scr "  bank 3"
		!byte $20, $74, $6a, $20, $74, $6a, $20, $74
		!byte $6a, $20, $74, $6a, $20, $74, $6a, $20
		!byte $74, $6a, $20, $74, $6a, $20, $74, $6a
		!byte $20, $74, $6a, $20, $74, $6a, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $4c, $7a, $20, $4c, $7a, $20, $4c
		!byte $7a, $20, $4c, $7a, $20, $4c, $7a, $20
		!byte $4c, $7a, $20, $4c, $7a, $20, $4c, $7a
		!byte $20, $4c, $7a, $20, $4c, $7a, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $35, $38, $20, $35, $39, $20, $36
		!byte $30, $20, $36, $31, $20, $36, $32, $20
		!byte $36, $33, $20, $36, $34, $20, $36, $35
		!byte $20, $37, $36, $20, $38, $32, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $4f, $50, $20, $4f, $50, $20, $4f
		!byte $50, $20, $4f, $50, $20, $4f, $50, $20
		!byte $4f, $50, $20, $4f, $50, $20, $4f, $50
		!byte $20, $4f, $50, $20, $4f, $50, $20, $20

		!scr "  bank 0"
		!byte $20, $74, $6a, $20, $74, $6a, $20, $74
		!byte $6a, $20, $74, $6a, $20, $74, $6a, $20
		!byte $74, $6a, $20, $74, $6a, $20, $74, $6a
		!byte $20, $74, $6a, $20, $74, $6a, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $4c, $7a, $20, $4c, $7a, $20, $4c
		!byte $7a, $20, $4c, $7a, $20, $4c, $7a, $20
		!byte $4c, $7a, $20, $4c, $7a, $20, $4c, $7a
		!byte $20, $4c, $7a, $20, $4c, $7a, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $37, $33, $20, $37, $32, $20, $37
		!byte $31, $20, $37, $30, $20, $36, $39, $20
		!byte $36, $38, $20, $36, $37, $20, $36, $36
		!byte $20, $37, $34, $20, $37, $35, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $4f, $50, $20, $4f, $50, $20, $4f
		!byte $50, $20, $4f, $50, $20, $4f, $50, $20
		!byte $4f, $50, $20, $4f, $50, $20, $4f, $50
		!byte $20, $4f, $50, $20, $4f, $50, $20, $20

		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $74, $6a, $20, $74, $6a, $20, $74
		!byte $6a, $20, $74, $6a, $20, $74, $6a, $20
		!byte $74, $6a, $20, $74, $6a, $20, $74, $6a
		!byte $20, $74, $6a, $20, $74, $6a, $20, $20

		!scr "fixed & "
		!byte $20, $4c, $7a, $20, $4c, $7a, $20, $4c
		!byte $7a, $20, $4c, $7a, $20, $4c, $7a, $20
		!byte $4c, $7a, $20, $4c, $7a, $20, $4c, $7a
		!byte $20, $4c, $7a, $20, $4c, $7a, $20, $20

		!scr "improved"
		!byte $20, $38, $33, $20, $38, $34, $20, $30
		!byte $34, $20, $31, $39, $20, $32, $30, $20
		!byte $18, $32, $20, $30, $32, $20, $32, $33
		!byte $20, $32, $34, $20, $38, $35, $20, $20

		!scr "vossi'20"
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20

		!byte $00, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20

!ifdef ROM{
*= $3fff
		!byte $00
}
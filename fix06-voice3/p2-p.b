; P500 diagnostic-test
; disassembled by Vossi 04/2020
; prepared for ACME reassembling
; fix01 - colorbug on diag-screen
; fix02 - ram search bug BMI skips also bank if CMP has negative check result in sub
; fix03 - search and show all ram banks bur test only 4
; fix04 - bank 0 fault not shown, bank 0 bit direction wrong
; fix05 - graphics char for ROM, (improved) ;)
; fix06 - use all three SID voices
!cpu 6502
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
; switches
;ROM = 0		; assemble extension rom
!ifdef 	ROM{!to "p2-p.bin", plain
} else{ 	!to "p2-p.prg", cbm }
; ***************************************** CONSTANTS *********************************************
FILL					= $aa		; fills free memory areas with $aa
SYSTEMBANK				= $0f		; systembank
BLACK					= $00		; color codes
GRAY1					= $0b
; ***************************************** ADDRESSES *********************************************
!addr CodeBank			= $00		; code bank register
!addr IndirectBank		= $01		; indirect bank register
!addr ScreenRAM			= $d000		; Screen RAM
!addr VIC				= $d800		; VIC register
; ***************************************** ZERO PAGE *********************************************
!addr pointer1			= $10		; 16bit pointer
!addr ext_color			= $1b		; exterior color
!addr delaycounter		= $1c		; 8bit counter for delay loop
!addr test_pages		= $25		; pages to test
!addr copy_target_bank	= $2c		; copy target bank
!addr copy_target		= $2d		; 16bit copy target address
!addr last_rambank		= $30		; last RAM bank
!addr copy_source_bank	= $32		; copy source bank
!addr copy_source		= $33		; 16bit copy source address
!addr counter			= $35		; counter
!addr colorpointer		= $36		; 16bit colorpointer
!addr test_mask			= $3a		; test			
!addr temp				= $43		; temp variable
;!addr check_source		= $44		; check source highbyte
;!addr check_target		= $45		; check target highbyte
!addr check				= $46		; check variable
!addr error				= $47		; error state
!addr pointer2			= $4e		; 16bit pointer
!addr sid_pointer		= $52		; SID register table
!addr tri1_pointer		= $52		; TRI1 register table - unused -
!addr tri2_pointer		= $62		; TRI2 register table - unused -
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
		ldy #$02						; clear zero page
		lda #$00
clrzplp:sta $0000,y
		iny
		bne clrzplp
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
		ldx #SYSTEMBANK					; set indirect bank to 15
		stx IndirectBank
		lda #BLACK						; set background color
		ldy #$00
		ldx #>VIC
		stx colorpointer+1
		ldx #$21						; VIC register #21
		stx colorpointer
		sta (colorpointer),y
		lda #GRAY1						; code will be increased to GRAY2 in sub
		sta ext_color
		jsr SetExteriorColor			; sub: set exterior color VIC register #20
		lda #$0c						; color $0c = light-grey
		ldx #$04						; 4 pages to fill
		stx counter
		ldx #$d4						; colorpointer to $d400 = start color RAM
		stx colorpointer+1
		ldx #$00
		stx colorpointer
		ldy #$00						; start with full page		original $ff ********* PATCHED *********
		jsr FillColor					; sub: fill memory - complete color RAM with light grey
		ldx #$01						; <1 page to fill
		stx counter
		ldx #$d4						; colorpointer to $d40b in color RAM
		stx colorpointer+1
		ldx #$0b
		stx colorpointer
		lda #$08						; color $08 = orange
		ldy #$11						; 17 bytes to fill
		jsr FillColor					; sub: fill memory - color line 0 column 12-28 = orange
		lda #$07						; color $07 = yellow
		ldy #$00						; byte 0
		ldx #$d4						; colorpointer to $d40b in color RAM
		stx colorpointer+1
		ldx #$0b
		stx colorpointer
		sta (colorpointer),y			; store yellow to line 0, column 11
		ldy #$12
		sta (colorpointer),y			; store yellow to line 0, column 29
		ldy #$13						; 19 bytes
		ldx #$32						; colorpointer to $d432
		stx colorpointer
		inc counter						; <1 page to fill
		jsr FillColor					; sub: fill memory - color line 1 column 11-29 = yellow
		dec colorpointer+1					; reset colorpointer highbyte to $d4
		lda #$01						; color $01 = white
		inc counter						; <1 page to fill
		ldx #$60						; colorpointer to $d460
		stx colorpointer
		ldy #$08						; 8 bytes
		jsr FillColor					; sub: fill memory - color line 2 column 17-25 = white
		lda #$03						; color $03 = cyan
		ldy #$1d						; 29 bytes
		inc counter						; <1 page to fill
		ldx #$d5						; colorpointer to $d520
		stx colorpointer+1
		ldx #$20
		stx colorpointer
		jsr FillColor					; sub: fill memory - color line 7 column 1-29
		inc counter
		dec colorpointer+1				; colorpointer to $d5c0
		ldx #$c0
		stx colorpointer
		ldy #$1d
		jsr FillColor					; sub: fill memory - color line 11 column 1-29
		inc counter						; <1 page to fill
		ldx #$d6						; colorpointer to $d660
		stx colorpointer+1
		ldx #$60
		stx colorpointer
		ldy #$1d
		jsr FillColor					; sub: fill memory - color line 15 column 1-29
		inc counter						; <1 page to fill
		ldx #$d7						; colorpointer to $d700
		stx colorpointer+1
		ldx #$00
		stx colorpointer
		ldy #$1d
		jsr FillColor					; sub: fill memory - color line 19 column 1-29
		inc counter						; <1 page to fill
		dec colorpointer+1				; colorpointer to $d7a0
		ldx #$a0
		stx colorpointer
		ldy #$1d
		jsr FillColor					; sub: fill memory - color line 23 column 1-29
		ldx #$00
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
		lda #$0d						; color $0d = light green
		sta (pointer1),y
		dec last_rambank				; decrease bank count to get last bank (first bank = 0)
		jmp Max4Banks					; jump cut to max 4 banks to test ********* PATCHED *********
; ----------------------------------------------------------------------------
; search for RAM - returns Z=1 if RAM found in page at pointer1
SearchRAM:
		clv								; why ?
		ldy #$00						; clear counter Y
		lda #$a5						; value = $ a5
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
		lda #$d8						; pointer = $d800 VIC  ********* PATCHED *********
		sta pointer1+1
		ldy #$00
		sty pointer1
		lda #SYSTEMBANK
		sta IndirectBank				; switch to bank 15
		ldy #$18						; VIC memory pointers register
		lda #$41						; clear bit1 CB11 = graphics character set
		sta (pointer1),y				; store to screen RAM
		lda #$d0						; pointer = $d000 screen RAM
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
		sta (colorpointer),y			; store in indirect bank
		dey
		bne FillColor					; next byte
		inc colorpointer+1				; removed wrong dey ********* PATCHED *********
		dec counter
		bne FillColor					; next page
		rts
; ----------------------------------------------------------------------------
; main test code
Main:	jsr CopySIDTable				; init zeropage $52-$8c from SIDTable
		lda #SYSTEMBANK
		sta IndirectBank				; switch to bank 15
		jsr PlaySound
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
		jsr Test					; sub: copy code and switch to new bank
		ldx #$03
		sed
		sec
l21ab:	lda $21,x
		adc #$00
		sta $21,x
		bcc l21b6
		dex
		bpl l21ab
l21b6:	lda #$1f						; pointer = $d01f 
		sta pointer1
		lda #$d0
		sta pointer1+1
		lda #SYSTEMBANK
		sta IndirectBank
		ldx #$ff
		ldy #$00
l21c6:	inx
		cpx #$04
		bne l21ce
		dex
		bne l21dc
l21ce:	lda $21,x
		beq l21c6
		and #$f0
		beq l21dc
l21d6:	jsr l26b1
		sta (pointer1),y
		iny
l21dc:	lda $21,x
		jsr Nibble2Screencode			; sub: convert nibble to digit screencode
		sta (pointer1),y
		iny
		inx
		lda $21,x
		cpx #$04
		bne l21d6
		cld
		jmp Main
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
		sta $4b							; remember target bank
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
		lda $4b
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
l2267:	stx $49
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
		ldx $49
		bpl l225c
l2282:	ldy CodeBank
		ldx $49
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
		lda #$00						; clear A, X
		tax
		ldy #$02
l22e8:	sty $42							; $42 = Y (start with $02)
		stx $41							; start with $00
		sta test_pages					; $00 = test all pages
		dey
		sty $26
		lda #$00
		sta pointer1					; pointer1 lowbyte = $00
		lda IndirectBank
		cmp #$0f						; check if target = bank 15
		beq notbnkf						; skip if not bank 15
		jsr PlaySound					; play sound
notbnkf:ldy $42							; start with Y = $02
		lda $41
		sta pointer1+1					; start with $0000
; test1 with upcounting value
test1nb:tya								; Y as test-byte
		sta $44							; remember test-byte
		sta (pointer1),y				; store test value to test-bank (start at $0002)
		lda (pointer1),y
		eor $44							; check test-byte
		and test_mask					; mask out bits to test - standard $ff = all tested
		beq test1ok
		jsr TestError					; jump to test error
test1ok:iny
		bne test1nb						; next byte
		inc pointer1+1					; increase highbyte
		lda pointer1+1
		cmp test_pages					; check if last test page
		bne test1nb						; next page
		jsr l261f
;
l2322:	tya
		sta $44
		lda (pointer1),y
		eor $44
		and test_mask
		beq l2330
		jsr TestError
l2330:	lda pointer1+1
		sta (pointer1),y
		lda (pointer1),y
		eor pointer1+1
		and test_mask
		beq l233f
		jsr TestError
l233f:	iny
		bne l2322
		inc pointer1+1
		lda pointer1+1
		cmp test_pages
		bne l2322
		jsr PlaySound
		jsr l261f
		lda #$55
		sta $44
		lda #$aa
		sta $45
l2358:	lda (pointer1),y
		eor pointer1+1
		and test_mask
		beq l2363
		jsr TestError
l2363:	lda #$55
		sta (pointer1),y
		lda (pointer1),y
		eor $44
		and test_mask
		beq l2372
		jsr TestError
l2372:	iny
		lda (pointer1),y
		eor pointer1+1
		and test_mask
		beq l237e
		jsr TestError
l237e:	lda #$aa
		sta (pointer1),y
		lda (pointer1),y
		eor $45
		and test_mask
		beq l238d
		jsr TestError
l238d:	iny
		bne l2358
		inc pointer1+1
		lda pointer1+1
		cmp test_pages
		bne l2358
		jsr l261f
l239b:	lda (pointer1),y
		eor $44
		and test_mask
		beq l23a6
		jsr TestError
l23a6:	lda #$aa
		sta (pointer1),y
		lda (pointer1),y
		eor $45
		and test_mask
		beq l23b5
		jsr TestError
l23b5:	iny
		lda (pointer1),y
		eor $45
		and test_mask
		beq l23c1
		jsr TestError
l23c1:	lda #$55
		sta (pointer1),y
		lda (pointer1),y
		eor $44
		and test_mask
		beq l23d0
		jsr TestError
l23d0:	iny
		bne l239b
		inc pointer1+1
		lda pointer1+1
		cmp test_pages
		bne l239b
		jsr PlaySound
		jsr l261f
		ldx #$5a
		stx check
l23e5:	lda (pointer1),y
		eor $45
		and test_mask
		beq l23f0
		jsr TestError
l23f0:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor check
		and test_mask
		beq l23fe
		jsr TestError
l23fe:	iny
		lda (pointer1),y
		eor $44
		and test_mask
		beq l240a
		jsr TestError
l240a:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor check
		and test_mask
		beq l2418
		jsr TestError
l2418:	iny
		bne l23e5
		inc pointer1+1
		lda pointer1+1
		cmp test_pages
		bne l23e5
		jsr l2626
		ldx #$5a
		stx $44
		ldx #$a5
		stx $45
l242e:	lda (pointer1),y
		eor $44
		and test_mask
		beq l2439
		jsr TestError
l2439:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor $45
		and test_mask
		beq l2447
		jsr TestError
l2447:	dey
		cpy #$ff
		bne l242e
		dec pointer1+1
		lda pointer1+1
		cmp $41
		bne l242e
l2454:	lda (pointer1),y
		eor $44
		and test_mask
		beq l245f
		jsr TestError
l245f:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor $45
		and test_mask
		beq l246d
		jsr TestError
l246d:	dey
		cpy $26
		bne l2454
		jsr PlaySound
		jsr l2626
		ldx #$5a
l247a:	lda (pointer1),y
		eor $45
		and test_mask
		beq l2485
		jsr TestError
l2485:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor $44
		and test_mask
		beq l2493
		jsr TestError
l2493:	dey
		cpy #$ff
		bne l247a
		dec pointer1+1
		lda pointer1+1
		cmp $41
		bne l247a
l24a0:	lda (pointer1),y
		eor $45
		and test_mask
		beq l24ab
		jsr TestError
l24ab:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor $44
		and test_mask
		beq l24b9
		jsr TestError
l24b9:	dey
		cpy $26
		bne l24a0
		jsr l261f
		ldx #$ff
		stx check
l24c5:	lda (pointer1),y
		eor $44
		and test_mask
		beq l24d0
		jsr TestError
l24d0:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor check
		and test_mask
		beq l24de
		jsr TestError
l24de:	iny
		bne l24c5
		inc pointer1+1
		lda pointer1+1
		cmp test_pages
		bne l24c5
		jsr PlaySound
		jsr l2626
		ldx #$00
		stx $44
		ldx #$ff
		stx $45
l24f7:	txa
		lda (pointer1),y
		eor $45
		and test_mask
		beq l2503
		jsr TestError
l2503:	sta (pointer1),y
		lda (pointer1),y
		eor $44
		and test_mask
		beq l2510
		jsr TestError
l2510:	dey
		cpy #$ff
		bne l24f7
		dec pointer1+1
		lda pointer1+1
		cmp $41
		bne l24f7
l251d:	txa
		lda (pointer1),y
		eor $45
		and test_mask
		beq l2529
		jsr TestError
l2529:	sta (pointer1),y
		lda (pointer1),y
		eor $44
		and test_mask
		beq l2536
		jsr TestError
l2536:	dey
		cpy $26
		bne l251d
		rts
; ----------------------------------------------------------------------------
; test error
TestError:
		clv
		sta $27							; remember wrong bits
		stx $28							; remember start higbyte
		sty $29							; remember lowbyte
		ldx IndirectBank
		stx $4b							; remember defective test bank
		ldy $4b							; Y = defective test bank
		cpy #$0f						; check if bank 15
		beq errbnkf						; skip if bank 15
										; removed dey to show bank 0 fault ********* PATCHED *********
		lda #$ff
		sta $0015,y						; store $ff to $15 + defective bank
		ldy $4b							; load defective testbank
										; removed dey to show bank 0 fault ********* PATCHED *********
		lda ErrorBarsLow,y				; load screen-pointer to faulty bank from table
		sta pointer2
		lda ErrorBarsHigh,y
		sta pointer2+1
		ldx #$08
		stx $49
l2564:	lda $27
		clc
		rol
		sta $27
		bcc l256f
		jsr l25dc
l256f:	lda #$03
		clc
		adc pointer2
		sta pointer2
		bcc l257a
		inc pointer2+1
l257a:	ldx $49
		dex
		stx $49
		bne l2564
l2581:	ldx $4b
		stx IndirectBank
		ldx $28
		ldy $29
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
		jsr l25dc
		bvc l2581
l259f:	lda pointer1+1
		cmp #$d4
		bpl l25cd
		lda $27
		and #$f0
		beq l25b8
		lda #$a4
		sta pointer2
		lda #$d2
		sta pointer2+1
		lda CodeBank
		jsr l25de
l25b8:	lda $27
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
l25dc:	lda #SYSTEMBANK
l25de:	sta IndirectBank
		ldy #$00
		lda (pointer2),y
		bmi l261e
		lda pointer2
		sta $50
		lda pointer2+1
		sta $51
		ldx #$03
l25f0:	ldy #$01
l25f2:	lda #$a0
		sta ($50),y
		dey
		bpl l25f2
		lda #$04
		clc
		adc $51
		sta $51
		ldy #$01
l2602:	lda #$07
		sta ($50),y
		dey
		bpl l2602
		lda $51
		sec
		sbc #$04
		sta $51
		lda #$28
		clc
		adc $50
		sta $50
		bcc l261b
		inc $51
l261b:	dex
		bne l25f0
l261e:	rts
l261f:	ldy $42
		lda $41
		sta pointer1+1
		rts
l2626:	ldy test_pages
		dey
		sty pointer1+1
		ldy #$ff
		rts
; ----------------------------------------------------------------------------
; memory copy sub - copies counter pages from source to target
CopyMemory:
		stx counter						; save pages to copy to counter $35, temp $43
		stx temp
		ldx IndirectBank				; save indirect bank to $4b
		stx $4b
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

		lda temp						; restore page to counter
		sta counter
		lda #$00						; clear $47
		sta error
		ldy $3f							; load start low byte
		lda $44							; check if $45 = $00
		ora $45
		and $45
		bne +							; skip if $45 not $00
		ldy #$48						; start at low byte $48
+		lda $44
		sta copy_source+1				; source high byte = $44
		lda $45
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
		ldx $4b							; restore indirect bank
		stx IndirectBank
		lda error						; return error state 0=ok, 1=error
		rts

l2699:	sta copy_source_bank
		stx copy_source+1
		stx $44
		sty copy_source
		rts
l26a2:	sta copy_target_bank
		stx copy_target+1
		stx $45
		sty copy_target
		rts
; ----------------------------------------------------------------------------
; Calc screencode digits for byte in A to AY
Hex2Screencode:	
		pha								; remember value on stack
		jsr Nibble2Screencode
		tay								; remember lower digit in Y
		pla								; restore value
l26b1:	lsr								; higher nibble -> lower nibble
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
		sta temp
		asl
		asl
		adc temp
		rts
; ----------------------------------------------------------------------------
; Set exterior color
SetExteriorColor:
		inc ext_color					; increase exterior color
		lda #>VIC
		sta colorpointer+1
		lda #$20						; VIC register #20
		sta colorpointer
		lda ext_color
		ldy #$00
		sta (colorpointer),y
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
		lda #tri2_pointer
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #<Tri2Table
		ldx #>Tri2Table
		ldy #$0f
		jsr CopyTable
		rts
; ----------------------------------------------------------------------------
; unused - copies Triport1 pointer to ZP
		lda #tri1_pointer
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #<Tri1Table
		ldx #>Tri1Table
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
		!byte $20, $d8, $21, $d8, $22, $d8, $23, $d8
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
Tri1Table:
		!byte $00, $de, $01, $de, $02, $de, $03, $de
		!byte $04, $de, $05, $de, $06, $de, $07, $de
; unused - Triport2 pointer copied to $62
Tri2Table:
		!byte $00, $df, $01, $df, $02, $df, $03, $df
		!byte $04, $df, $05, $df, $06, $df, $07, $df

; ----------------------------------------------------------------------------
; reduce last bank to test = max 3
Max4Banks:												; ********* PATCHED *********
		lda last_rambank
		cmp #$04
		bmi max4bnk
		lda #$03
		sta last_rambank
max4bnk:jmp Main
; ************************************* ZONE SCREENDATA *******************************************
!zone screendata
*= $3000
ScreenData:
		!scr "rambanks"									; ********* PATCHED *********
		!byte $20, $20, $20, $65, $10, $05, $14, $20
		!byte $09, $09, $20, $04, $09, $01, $07, $0e
		!byte $0f, $13, $14, $09, $03, $67, $20, $20
		!scr "  cycles"									; ********* PATCHED *********

		!byte $20, $20, $20, $20, $20, $20, $20, $20
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

		!byte $20, $20, $20, $20, $20, $20, $20, $20
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
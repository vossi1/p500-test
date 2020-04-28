; P500 diagnostic-test
; disassembled by Vossi 04/2020
; prepared for ACME reassembling
; fix01 - colorbug on diag-screen
; fix02 - ram search bug BMI skips also bank if CMP has negative check result in sub
; fix03 - search and show allr am banks bur test only 4
!cpu 6502
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
!to "p2-p.prg", cbm 
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
!addr copy_target_bank	= $2c		; copy target bank
!addr copy_target		= $2d		; 16bit copy target address
!addr rambanks			= $30		; found RAM banks
!addr copy_source_bank	= $32		; copy source bank
!addr copy_source		= $33		; 16bit copy source address
!addr counter			= $35		; counter
!addr colorpointer		= $36		; 16bit colorpointer
!addr temp				= $43		; temp variable
;!addr check_source		= $44		; check source highbyte
;!addr check_target		= $45		; check target highbyte
!addr check				= $46		; check variable
!addr error				= $47		; error state
!addr pointer2			= $4e		; 16bit pointer
;						= $52		; table?
; ***************************************** ZONE MAIN *********************************************
!zone main
!initmem FILL
*= $2000
		sei								; disable interrupts
		cld								; clear decimal flag
		ldx #$ff
		txs								; reset stack pointer
		ldy #$02						; clear zero page
		lda #$00
clrzplp:sta $0000,y
		iny
		bne clrzplp
		jsr ClearScreen					; clear screen
		lda #$30						; source = $3000
		sta copy_source+1
		lda #$00
		sta copy_source
		lda #>ScreenRAM					; target = $d000
		sta copy_target+1
		lda #<ScreenRAM
		sta copy_target
		lda CodeBank							; source = active code bank
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
		inc rambanks					; increase RAM bank present counter
noram:	inx								; increase bank
		cpx #$0f						; check if last possible RAM bank	orig. $04 ********* PATCHED *********
		bne findram						; search next RAM bank
		lda rambanks					; load RAM banks 		
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
		dec rambanks					; decrease RAM bank count to highest bank (first bank = 0)
		jmp Max4Banks					; jump cut to max 4 banks to test ********* PATCHED *********
; ----------------------------------------------------------------------------
; search for RAM - returns Z=1 if RAM found in page at pointer1
SearchRAM:
		clv
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
; Clear screen
ClearScreen:
		lda #$d0
		sta pointer1+1
		ldy #$00
		sty pointer1
		lda #SYSTEMBANK
		sta IndirectBank
		lda #$20
		ldx #$04
clrscr: sta (pointer1),y
		iny
		bne clrscr
		inc pointer1+1
		dex
		bne clrscr
		rts
; ----------------------------------------------------------------------------
; Fills color memory Y bytes, counter pages with A
FillColor:
		sta (colorpointer),y			; store in indirect bank
		dey
		bne FillColor					; next byte
		inc colorpointer+1						; removed dey ********* PATCHED *********
		dec counter
		bne FillColor					; next page
		rts
; ----------------------------------------------------------------------------
; main test code
test:	jsr InitZeropage				; init zeropage $52-$8c from InitTable1
		lda #SYSTEMBANK
		sta IndirectBank				; switch to bank 15
		jsr l21f1
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
		jsr CopyCode					; sub: copy code and switch to new bank
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
		jmp test
; ----------------------------------------------------------------------------
; Dummy subroutine
DummySub:
		rts
		rti
; ----------------------------------------------------------------------------
l21f1:	ldy #$00
		lda IndirectBank
		sta $4b
		lda #SYSTEMBANK
		sta IndirectBank
		sta ($82),y
		lda #$1a
		sta ($6a),y
		lda #$0a
		sta ($6c),y
		lda #$4e
		sta ($62),y
		lda #$20
		sta ($60),y
		lda #$07
		sta ($54),y
		lda #$d0
		sta ($52),y
		lda #$15
		sta ($68),y
		lda #$14
		sta ($68),y
		lda $4b
		sta IndirectBank
		rts
; ----------------------------------------------------------------------------
; copy code and switch to new bank
CopyCode:
		lda #$ff
		sta $3a
		ldy rambanks
		sty $4a
		ldx CodeBank
		stx copy_source_bank
		dex
		bpl notbnk0
		ldx rambanks
notbnk0:stx copy_target_bank
		stx $31
		ldx copy_target_bank
		stx IndirectBank
		jsr l22e3
		ldx copy_target_bank
		stx copy_source_bank
		dex
		bpl l2247
		ldx rambanks
l2247:	stx copy_target_bank
		dec $4a
		bne notbnk0
		ldy rambanks
		sty $4a
		ldx CodeBank
		dex
		bpl l2258
		ldx rambanks
l2258:	lda $15,x
		beq l2267
l225c:	dex
		bpl l2261
		ldx rambanks
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
		jsr l22e3
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
		sta $3a
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
l22e3:	lda #$00
		tax
		ldy #$02
l22e8:	sty $42
		stx $41
		sta $25
		dey
		sty $26
		lda #$00
		sta pointer1
		lda IndirectBank
		cmp #$0f
		beq l22fe
		jsr l21f1
l22fe:	ldy $42
		lda $41
		sta pointer1+1
l2304:	tya
		sta $44
		sta (pointer1),y
		lda (pointer1),y
		eor $44
		and $3a
		beq l2314
		jsr l253c
l2314:	iny
		bne l2304
		inc pointer1+1
		lda pointer1+1
		cmp $25
		bne l2304
		jsr l261f
l2322:	tya
		sta $44
		lda (pointer1),y
		eor $44
		and $3a
		beq l2330
		jsr l253c
l2330:	lda pointer1+1
		sta (pointer1),y
		lda (pointer1),y
		eor pointer1+1
		and $3a
		beq l233f
		jsr l253c
l233f:	iny
		bne l2322
		inc pointer1+1
		lda pointer1+1
		cmp $25
		bne l2322
		jsr l21f1
		jsr l261f
		lda #$55
		sta $44
		lda #$aa
		sta $45
l2358:	lda (pointer1),y
		eor pointer1+1
		and $3a
		beq l2363
		jsr l253c
l2363:	lda #$55
		sta (pointer1),y
		lda (pointer1),y
		eor $44
		and $3a
		beq l2372
		jsr l253c
l2372:	iny
		lda (pointer1),y
		eor pointer1+1
		and $3a
		beq l237e
		jsr l253c
l237e:	lda #$aa
		sta (pointer1),y
		lda (pointer1),y
		eor $45
		and $3a
		beq l238d
		jsr l253c
l238d:	iny
		bne l2358
		inc pointer1+1
		lda pointer1+1
		cmp $25
		bne l2358
		jsr l261f
l239b:	lda (pointer1),y
		eor $44
		and $3a
		beq l23a6
		jsr l253c
l23a6:	lda #$aa
		sta (pointer1),y
		lda (pointer1),y
		eor $45
		and $3a
		beq l23b5
		jsr l253c
l23b5:	iny
		lda (pointer1),y
		eor $45
		and $3a
		beq l23c1
		jsr l253c
l23c1:	lda #$55
		sta (pointer1),y
		lda (pointer1),y
		eor $44
		and $3a
		beq l23d0
		jsr l253c
l23d0:	iny
		bne l239b
		inc pointer1+1
		lda pointer1+1
		cmp $25
		bne l239b
		jsr l21f1
		jsr l261f
		ldx #$5a
		stx check
l23e5:	lda (pointer1),y
		eor $45
		and $3a
		beq l23f0
		jsr l253c
l23f0:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor check
		and $3a
		beq l23fe
		jsr l253c
l23fe:	iny
		lda (pointer1),y
		eor $44
		and $3a
		beq l240a
		jsr l253c
l240a:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor check
		and $3a
		beq l2418
		jsr l253c
l2418:	iny
		bne l23e5
		inc pointer1+1
		lda pointer1+1
		cmp $25
		bne l23e5
		jsr l2626
		ldx #$5a
		stx $44
		ldx #$a5
		stx $45
l242e:	lda (pointer1),y
		eor $44
		and $3a
		beq l2439
		jsr l253c
l2439:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor $45
		and $3a
		beq l2447
		jsr l253c
l2447:	dey
		cpy #$ff
		bne l242e
		dec pointer1+1
		lda pointer1+1
		cmp $41
		bne l242e
l2454:	lda (pointer1),y
		eor $44
		and $3a
		beq l245f
		jsr l253c
l245f:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor $45
		and $3a
		beq l246d
		jsr l253c
l246d:	dey
		cpy $26
		bne l2454
		jsr l21f1
		jsr l2626
		ldx #$5a
l247a:	lda (pointer1),y
		eor $45
		and $3a
		beq l2485
		jsr l253c
l2485:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor $44
		and $3a
		beq l2493
		jsr l253c
l2493:	dey
		cpy #$ff
		bne l247a
		dec pointer1+1
		lda pointer1+1
		cmp $41
		bne l247a
l24a0:	lda (pointer1),y
		eor $45
		and $3a
		beq l24ab
		jsr l253c
l24ab:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor $44
		and $3a
		beq l24b9
		jsr l253c
l24b9:	dey
		cpy $26
		bne l24a0
		jsr l261f
		ldx #$ff
		stx check
l24c5:	lda (pointer1),y
		eor $44
		and $3a
		beq l24d0
		jsr l253c
l24d0:	txa
		sta (pointer1),y
		lda (pointer1),y
		eor check
		and $3a
		beq l24de
		jsr l253c
l24de:	iny
		bne l24c5
		inc pointer1+1
		lda pointer1+1
		cmp $25
		bne l24c5
		jsr l21f1
		jsr l2626
		ldx #$00
		stx $44
		ldx #$ff
		stx $45
l24f7:	txa
		lda (pointer1),y
		eor $45
		and $3a
		beq l2503
		jsr l253c
l2503:	sta (pointer1),y
		lda (pointer1),y
		eor $44
		and $3a
		beq l2510
		jsr l253c
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
		and $3a
		beq l2529
		jsr l253c
l2529:	sta (pointer1),y
		lda (pointer1),y
		eor $44
		and $3a
		beq l2536
		jsr l253c
l2536:	dey
		cpy $26
		bne l251d
		rts
l253c:	clv
		sta $27
		stx $28
		sty $29
		ldx IndirectBank
		stx $4b
		ldy $4b
		cpy #$0f
		beq l258c
		dey
		lda #$ff
		sta $0015,y
		ldy $4b
		dey
		lda l2822,y
		sta pointer2
		lda l2826,y
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
l258c:	lda pointer1+1
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
l2626:	ldy $25
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
; 
		jsr l26c9
		asl
		asl
		rts
l26c9:	clc
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
		ldy #$ff
		ldx #$ff
l26e6:	dey
		bne l26e6
		dex
		bne l26e6
		dec $1c
		bne l26e6
		rts
; ----------------------------------------------------------------------------
		lda #$72
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #$e2
		ldx #$28
		ldy #$1f
		jsr CopyTable
		rts
; ----------------------------------------------------------------------------
		lda #$62
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #$1a
		ldx #$29
		ldy #$0f
		jsr CopyTable
		rts
; ----------------------------------------------------------------------------
		lda #$52
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #$0a
		ldx #$29
		ldy #$0f
		jsr CopyTable
		rts
; ----------------------------------------------------------------------------
		lda #$92
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #$02
		ldx #$29
		ldy #$07
		jsr CopyTable
		rts
; ----------------------------------------------------------------------------
; init zeropage
InitZeropage:
		lda #$52						; pointer = $0052 
		sta pointer1
		lda #$00
		sta pointer1+1
		lda #<InitTable1				; XA = InitTable1
		ldx #>InitTable1
		ldy #$39						; bytes to copy = $00-$39
		jsr CopyTable					; sub: copy table
		rts
; ----------------------------------------------------------------------------
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
*= $278a
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

l2822:	!byte $a9, $49, $c1, $89

l2826:	!byte $d0, $d1, $d1, $d2

;282a
!scr " * *  BAD PROGRAM CHECKSUM  * * "

;284a
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

InitTable1:
		!byte $00, $da, $01, $da, $02, $da, $03, $da
		!byte $04, $da, $05, $da, $06, $da, $07, $da
		!byte $08, $da, $09, $da, $0a, $da, $0b, $da
		!byte $0c, $da, $0d, $da, $0e, $da, $0f, $da
		!byte $10, $da, $11, $da, $12, $da, $13, $da
		!byte $14, $da, $15, $da, $16, $da, $17, $da
		!byte $18, $da, $19, $da, $1a, $da, $1b, $da
		!byte $1c, $da

		!byte $00, $dc, $01, $dc, $02, $dc, $03, $dc
		!byte $04, $dc, $05, $dc, $06, $dc, $07, $dc
		!byte $08, $dc, $09, $dc, $0a, $dc, $0b, $dc
		!byte $0c, $dc, $0d, $dc, $0e, $dc, $0f, $dc
		!byte $00, $dd, $01, $dd, $02, $dd, $03, $dd
		!byte $00, $de, $01, $de, $02, $de, $03, $de
		!byte $04, $de, $05, $de, $06, $de, $07, $de
		!byte $00, $df, $01, $df, $02, $df, $03, $df
		!byte $04, $df, $05, $df, $06, $df, $07, $df

; ----------------------------------------------------------------------------
; reduce last bank to test = max 3
Max4Banks:												; ********* PATCHED *********
		lda rambanks
		cmp #$04
		bmi max4bnk
		lda #$03
		sta rambanks
max4bnk:jmp test
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
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
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
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $36, $36, $20, $36, $37, $20, $36
		!byte $38, $20, $36, $39, $20, $37, $30, $20
		!byte $37, $31, $20, $37, $32, $20, $37, $33
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
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $38, $33, $20, $38, $34, $20, $30
		!byte $34, $20, $31, $39, $20, $32, $30, $20
		!byte $18, $32, $20, $30, $32, $20, $32, $33
		!byte $20, $32, $34, $20, $38, $35, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $00, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20
		!byte $20, $20, $20, $20, $20, $20, $20, $20

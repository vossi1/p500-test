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
; fix09 - removed untested chip-numbers - small fix: adr errorbar bank 3 lobyte wrong
; first final 1.0
; added TOD-Test 1.1
; added ROM Checksums 1.2
; added Timer Tests 1.3
!cpu 6502
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
; switches
;VICE = 1	; 2 Rambanks for Vice (RAM detection doesn't work properly in Vice)
;ROM = 0	; assemble extension rom
!ifdef 	ROM{!to "p2-p.bin", plain
} else{ 	!to "p2-p.prg", cbm }
; ***************************************** IMPORTANT *********************************************
CODESIZE		= $30		; Codesize to copy from bank to bank !!!
; ***************************************** CONSTANTS *********************************************
FILL			= $aa		; fills free memory areas with $aa
SYSTEMBANK		= $0f		; systembank
REVSPACE		= $a0		; reverse space for faulty chip
BLACK			= $00		; color codes
WHITE			= $01
CYAN			= $03
YELLOW			= $07
ORANGE 			= $08
LIGHTRED		= $0a
GRAY1			= $0b
GRAY2			= $0c
LIGHTGREEN		= $0d
; VIC register
MEMPTR			= $18		; memory pointers
EXTCOL			= $20		; exterior color
BGRCOL			= $21		; background color
; TPI register
PC			= $2 *2		; port c
MIR			= $5 *2		; interrupt mask register
CR			= $6 *2		; control register
; CIA register
TALO			= $4 *2		; timer a lo
TAHI			= $5 *2		; timer a hi
TBLO			= $6 *2		; timer b lo
TBHI			= $7 *2		; timer b hi
TOD10			= $8 *2		; tod 10th of seconds
TODSEC			= $9 *2		; tod seconds
TODMIN			= $a *2		; tod monutes
TODHR			= $b *2		; tod hours
ICR			= $d *2		; interrupt control register
CRA			= $e *2		; control register b
CRB			= $f *2		; control register b
; SID register
OSC1			= $00 *2	; oscillator 1
OSC2			= $07 *2	; oscillator 2
OSC3			= $0e *2	; oscillator 3
FREQLO			= $00 *2	; frequency lo
FREQHI			= $01 *2	; frequency hi
OSCCTL			= $04 *2	; oscillator control
ATKDCY			= $05 *2	; attack/decay
SUSREL			= $06 *2	; sustain/release
VOLUME			= $18 *2	; volume
; ***************************************** ADDRESSES *********************************************
!addr CodeBank		= $00		; code bank register
!addr IndirectBank	= $01		; indirect bank register
!addr MemZero		= $0000
!addr ScreenRAM		= $d000		; Screen RAM
!addr ColorRAM		= $d400		; Color RAM
!addr VIC		= $d800		; VIC address
!addr HW_NMI		= $fffa		; system NMI vector
!addr HW_RESET		= $fffc		; system RESET vector
!addr HW_IRQ		= $fffe		; system IRQ vector
; ***************************************** ZERO PAGE *********************************************
!addr pointer1		= $10		; 16bit pointer
!addr bank_state	= $15 ; - $18	; bank faulty state (max 4 banks)
!addr bank_state_full	= $0015
!addr ext_color		= $1b		; exterior color
!addr cycles		= $21 ; - $24	; cycles counter decimal for 8 digits
!addr test_pages	= $25		; pages to test
!addr temp2		= $26		; temp variable
!addr faulty_bits	= $27		; faulty test bits
!addr storage1		= $28		; temp storage
!addr storage2		= $29		; temp storage
!addr copy_target_bank	= $2c		; copy target bank
!addr copy_target	= $2d		; 16bit copy target address
!addr last_rambank	= $30		; last RAM bank
!addr copy_source_bank	= $32		; copy source bank
!addr copy_source	= $33		; 16bit copy source address
!addr counter		= $35		; counter
!addr color_pointer	= $36		; 16bit colorpointer
!addr test_mask		= $3a		; test mask (to test only 4 bit in color RAM)	
!addr screen_pos	= $3b		; screen pos rom checksums
!addr temp_count_sum	= $3c		; temp rom checksums
!addr temp6		= $3f		; temp6	
!addr start_high	= $41		; test start address highbyte	
!addr start_low		= $42		; test start address lowbyte	
!addr temp1		= $43		; temp variable
!addr temp3		= $44		; temp variable
!addr temp4		= $45		; temp variable
!addr check		= $46		; check variable
!addr error		= $47		; error state
!addr temp7		= $48		; temp timer test
!addr temp5		= $49		; temp variable
!addr banks_counter	= $4a		; counter for banks to test in a cycle
!addr temp_bank		= $4b		; temp bank variable
!addr temp_dec_value	= $4c		; temp timertest
!addr temp_irq		= $4d		; temp irq
!addr pointer2		= $4e		; 16bit pointer
!addr pointer3		= $50		; 16bit pointer
!addr sid		= $52 ; -$91	; SID register table
!addr tpi1		= $92 ; -$a1	; TPI1 register table - unused -
!addr tpi2		= $92 ; -$a1	; TPI2 register table - unused -
!addr cia		= $92 ; -$b1	; CIA register table
!addr acia		= $92 ; -$99	; ACIA register table - unused -
!addr time1_hours	= $b2		; time 1 hours
!addr time1_minutes	= $b3		; time 1 minutes
!addr time1_seconds	= $b4		; time 1 seconds
!addr time1_10th	= $b5		; time 1 10th seconds
!addr time2_hours	= $b6		; time 2 hours
!addr time2_minutes	= $b7		; time 2 minutes
!addr time2_seconds	= $b8		; time 2 seconds
!addr time2_10th	= $b9		; time 2 10th seconds
!addr tod_count1	= $ba		; tod test counter
!addr tod_count2	= $bb		; tod test counter
!addr tod_count3	= $bc		; tod test counter
!addr tod_state		= $bd		; TOD state - $00 ok, $ff = bad
!addr timer_state	= $bd		; timer state - $00 = ok
!addr nmi_pointer	= $f0		; nmi pointer
!addr reset_pointer	= $f2		; reset pointer
!addr irq_pointer	= $f4		; irq pointer
; ***************************************** ZONE MAIN *********************************************
!zone main
!initmem FILL
*= $2000
	jmp Start			; jump to start
	jmp Start			; jump to start
	!byte $c3,$c2,$cd,"2"		; cbm-rom ident-bytes 'C'= with init, 'BM', '2' = 4k-block 2
Start:	
	sei				; disable interrupts
	cld				; clear decimal flag
	ldx #$ff
	txs				; reset stack pointer
; init
	ldy #$02			; clear zero page
	lda #$00
clrzplp:sta MemZero,y			; $0000
	iny
	bne clrzplp
; draw screen
	jsr ClearScreen			; clear screen and select graphics character set
	lda #>ScreenData		; source = ScreenData
	sta copy_source+1
	lda #<ScreenData
	sta copy_source
	lda #>ScreenRAM			; target = ScreenRAM
	sta copy_target+1
	lda #<ScreenRAM
	sta copy_target
	lda CodeBank			; source = active code bank
	sta copy_source_bank
	lda #SYSTEMBANK			; target = Bank 15
	sta copy_target_bank
	ldx #$04			; 4 pages to copy
	jsr CopyMemory			; sub: draw screen
; color screen
	ldx #SYSTEMBANK			; set indirect bank to 15
	stx IndirectBank
	lda #BLACK			; set background color
	ldy #$00
	ldx #>VIC
	stx color_pointer+1
	ldx #BGRCOL			; background color
	stx color_pointer
	sta (color_pointer),y
	lda #GRAY1			; code will be increased to GRAY2 in sub
	sta ext_color
	jsr SetExteriorColor		; sub: set exterior color
	lda #GRAY2
	ldx #$04			; 4 pages to fill
	stx counter
	ldx #$d4			; colorpointer to $d400 = start color RAM
	stx color_pointer+1
	ldx #$00
	stx color_pointer
	ldy #$00			; start with full page		original $ff ********* PATCHED *********
	jsr FillColor			; sub: fill memory - complete color RAM with light grey
	ldx #$01			; <1 page to fill
	stx counter
	ldx #$d4			; colorpointer to $d40b in color RAM
	stx color_pointer+1
	ldx #$0b
	stx color_pointer
	lda #ORANGE
	ldy #$11			; 17 bytes to fill
	jsr FillColor			; sub: fill memory - color line 0 column 12-28 = orange
	lda #YELLOW
	ldy #$00			; byte 0
	ldx #$d4			; colorpointer to $d40b in color RAM
	stx color_pointer+1
	ldx #$0b
	stx color_pointer
	sta (color_pointer),y		; store yellow to line 0, column 11
	ldy #$12
	sta (color_pointer),y		; store yellow to line 0, column 29
	ldy #$13			; 19 bytes
	ldx #$32			; colorpointer to $d432
	stx color_pointer
	inc counter			; <1 page to fill
	jsr FillColor			; sub: fill memory - color line 1 column 11-29 = yellow
	dec color_pointer+1		; reset colorpointer highbyte to $d4
	lda #WHITE
	inc counter			; <1 page to fill
	ldx #$60			; colorpointer to $d460
	stx color_pointer
	ldy #$08			; 8 bytes
	jsr FillColor			; sub: fill memory - color line 2 column 17-25 = white
	lda #CYAN
	ldy #$1d			; 29 bytes
	inc counter			; <1 page to fill
	ldx #$d5			; colorpointer to $d520
	stx color_pointer+1
	ldx #$20
	stx color_pointer
	jsr FillColor			; sub: fill memory - color line 7 column 1-29
	inc counter
	dec color_pointer+1		; colorpointer to $d5c0
	ldx #$c0
	stx color_pointer
	ldy #$1d
	jsr FillColor			; sub: fill memory - color line 11 column 1-29
	inc counter			; <1 page to fill
	ldx #$d6			; colorpointer to $d660
	stx color_pointer+1
	ldx #$60
	stx color_pointer
	ldy #$1d
	jsr FillColor			; sub: fill memory - color line 15 column 1-29
	inc counter			; <1 page to fill
	ldx #$d7			; colorpointer to $d700
	stx color_pointer+1
	ldx #$00
	stx color_pointer
	ldy #$1d
	jsr FillColor			; sub: fill memory - color line 19 column 1-29
	inc counter			; <1 page to fill
	dec color_pointer+1		; colorpointer to $d7a0
	ldx #$a0
	stx color_pointer
	ldy #$1d
	jsr FillColor			; sub: fill memory - color line 23 column 1-29
	ldx #$00
; search RAM banks
findram:stx IndirectBank		; switch to indirect bank 0
	lda #$08			; pointer1 to $0800
	sta pointer1+1
	lda #$00
	sta pointer1
	jsr SearchRAM			; sub: search for RAM - Z=1 RAM found
	bmi noram			; skip if no RAM found
	inc last_rambank		; increase RAM bank present counter
noram:	inx				; increase bank
	cpx #$0f			; check if last possible RAM bank	orig. $04 ********* PATCHED *********
	bne findram			; search next RAM bank
!ifdef VICE{
	lda #2			; ********* 2 Rambanks for Vice *********
	sta last_rambank
} else{
	lda last_rambank		; load RAM banks
}
; write found banks to screen
	jsr Hex2Screencode		; sub: calc screencode digits for byte in A to AY
	lda #<(ScreenRAM+9)		; pointer to screen position
	sta pointer1
	lda #>(ScreenRAM+9)
	sta pointer1+1
	tya				; move lower digit to A
	ldy #SYSTEMBANK
	sty IndirectBank		; switch to bank 15
	ldy #$00
	sta (pointer1),y		; write RAM banks count to screen line 0 column 9
	lda pointer1+1
	clc
	adc #$04			; add $04 to highbyte pointer1 = color RAM
	sta pointer1+1
	lda #LIGHTGREEN
	sta (pointer1),y		; store color for banks found
	ldy #$28			; add 40 for next line			********* PATCHED *********
	lda #LIGHTRED
	sta (pointer1),y		; store color for testbank
	dec last_rambank		; decrease bank count to get last bank (first bank = 0)
	lda last_rambank		; check if more than 4 RAM banks ********* PATCHED *********
	cmp #$04
	bmi max4bnk			; skip if last bank is <= 3
	lda #$03			; reduce last bank to test = 3
	sta last_rambank
max4bnk:jmp Main			; jump to main code
; ----------------------------------------------------------------------------
; search for RAM - returns Z=1 if RAM found in page at pointer1
SearchRAM:
	clv				; why ?
	ldy #$00			; clear counter Y
	lda #$a5			; value = $a5
framsto:sta (pointer1),y		; store to RAM				
	iny
	bne framsto			; next byte
framchk	lda (pointer1),y		; load from RAM
	cmp #$a5			; check value
	beq framfnd			; end check if RAM found N=0
	iny
	bne framchk			; next byte if not equal
	dey				; Y=$ff -> N=1 / no RAM found
framfnd	rts				; N=0 RAM found
; ----------------------------------------------------------------------------
; Clear screen and select graphics character set
ClearScreen:
	lda #>VIC			; pointer = $d800 VIC  ********* PATCHED *********
	sta pointer1+1
	ldy #<VIC
	sty pointer1
	lda #SYSTEMBANK
	sta IndirectBank		; switch to bank 15
	ldy #MEMPTR			; VIC memory pointers register
	lda #$41			; clear bit1 CB11 = graphics character set
	sta (pointer1),y		; store to screen RAM
	lda #>ScreenRAM			; pointer to screen RAM
	sta pointer1+1
	lda #$20			; fill with <SPACE>
	ldx #$04			; 4 pages to clear
clrscr: sta (pointer1),y		; store to screen RAM
	iny
	bne clrscr			; next byte
	inc pointer1+1
	dex
	bne clrscr			; next page
	rts
; ----------------------------------------------------------------------------
; Fills color memory Y bytes, counter pages with A
FillColor:
	sta (color_pointer),y		; store in indirect bank
	dey
	bne FillColor			; next byte
	inc color_pointer+1		; removed wrong dey ********* PATCHED *********
	dec counter
	bne FillColor			; next page
	rts
; ----------------------------------------------------------------------------
; Main loop
Main:	
	jsr InitSIDPointer		; init sid pointer
	lda #SYSTEMBANK
	sta IndirectBank		; switch to bank 15
	jsr PlaySound			; play sound
	jsr SetExteriorColor		; increase exterior color after each cycle
	jsr ROMChecksums		; calc and print ROM checksums
	jsr TimerTest			; timer test
	jsr TODTest			; tod test				********** EXTENDED **********
	jsr DummySub			; Call 19x dummy-subroutine
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
	jsr Test			; sub: test, copy code, switch to new bank
; increase cycles counter
	ldx #$03			; 3 digits for cycles
	sed				; switch do decimal mode
	sec				; set carry to add 1 cycle
mnxtdig:lda cycles,x			; start with last digit
	adc #$00			; use A with ADC instead of INX/INY because indexed access
	sta cycles,x			; increase last digit $23... -> $21 if 0
	bcc mdign00			; skip if not 00
	dex
	bpl mnxtdig			; next digit if 00
; write cycles to screen
mdign00:lda #<(ScreenRAM+40+32)		; pointer to screen position
	sta pointer1
	lda #>(ScreenRAM+40+32)
	sta pointer1+1			; set screen pointer to cycles counter
	lda #SYSTEMBANK
	sta IndirectBank		; switch to systembank
	ldx #$ff
	ldy #$00
mskpzer:inx
	cpx #$04
	bne mnotlst			; check digit if not last digit
	dex				; if last digit was zero, decrease X to print 0
	bne mprtdig
mnotlst:lda cycles,x
	beq mskpzer
	and #$f0			; clear lower nibble
	beq mprtdig			; print if < 10
mprtnxt:jsr UpperNibble2Screencode	; sub: convert upper nibble to digit screencode
	sta (pointer1),y		; print upper nibble
	iny
mprtdig:lda cycles,x			; load digit again
	jsr Nibble2Screencode		; sub: convert nibble to digit screencode
	sta (pointer1),y		; write digit to screen
	iny				; increase screen pointer to next place
	inx				; increase digit counter
	lda cycles,x			; load next digit
	cpx #$04
	bne mprtnxt			; print next digit
	cld				; clear decimal flag
	jmp Main			; next test cycle
; ----------------------------------------------------------------------------
; Dummy subroutine
DummySub:
	rts
; ----------------------------------------------------------------------------
; play sound
PlaySound:
	ldy #$00			; clear Y for indirect writes
	lda IndirectBank
	sta temp_bank			; remember target bank
	lda #SYSTEMBANK
	sta IndirectBank		; indirect bank = bank 15
	sta (sid+VOLUME),y		; volume = 15 (A already 15)
	lda #$1a
	sta (sid+OSC2+ATKDCY),y		; voice 2 AD
	lda #$0a
	sta (sid+OSC2+SUSREL),y		; voice 2 SR
	lda #$4e			; frequency = 200000 ~ note D#6
	sta (sid+OSC2+FREQHI),y		; voice 2 frequency hi
	lda #$20
	sta (sid+OSC2+FREQLO),y		; voice 2 frequency low
	lda #$18
	sta (sid+OSC3+ATKDCY),y		; voice 3 AD	with 3 voices ********* PATCHED *********
	lda #$0a
	sta (sid+OSC3+SUSREL),y		; voice 3 SR
	lda #$09			; frequency = 2500
	sta (sid+OSC3+FREQHI),y		; voice 3 frequency hi
	lda #$c4
	sta (sid+OSC3+FREQLO),y		; voice 3 frequency low
	lda #$07			; frequency = 2000 ~ note B2
	sta (sid+OSC1+FREQHI),y		; voice 1 frequency hi
	lda #$d0
	sta (sid+OSC1+FREQLO),y		; voice 1 frequency low
	lda #$15
	sta (sid+OSC2+OSCCTL),y		; voice 2 triangle,ringmod,gate
	lda #$21
	sta (sid+OSC3+OSCCTL),y		; voice 3 triangle, gate
	lda #$14
	sta (sid+OSC2+OSCCTL),y		; voice 2 triangle, ringmod
	lda #$20
	sta (sid+OSC3+OSCCTL),y		; voice 3 triangle
	lda temp_bank
	sta IndirectBank		; restore target bank
	rts
; ----------------------------------------------------------------------------
; calc and print rom checksums
ROMChecksums:
	ldx #0				; "CHECKSUMS"
	jsr DrawMessage			; sub: draw message
	lda #SYSTEMBANK
	sta IndirectBank		; systembank
	ldy #$00			; first screen position
	ldx #$20			; rom size 2000
	lda #$e0			; kernal address e000
	jsr prntrom			; calculate and print checksum of one rom
	ldy #$03			; screen pos basic hi
	ldx #$20
	lda #$a0			; basic hi address a000
	jsr prntrom			; calculate and print checksum of one rom
	ldy #$06
	ldx #$20
	lda #$80			; basic hi address 8000
; calculate and print checksum of rom
prntrom:sty screen_pos
	sta pointer1+1			; set pointer hi to rom start
	dex
	txa
	clc
	adc pointer1+1			; calc rom end 
	sta pointer1+1
	lda #$00
	sta temp_count_sum		; init sum
	sta pointer1
	tay
rsumlp:	clc
	lda (pointer1),y		; load byte
	adc temp_count_sum		; add previous value
	adc #$00			; add carry
	adc #$00			; ***** why ?
	sta temp_count_sum		; store new sum
	dey
	bne rsumlp			; next byte
	dec pointer1+1
	dex
	bpl rsumlp			; next page
; print sum
	lda #<(ScreenRAM+40*21+9)	; screen position lo
	clc
	adc screen_pos			; screen pos add value
	sta pointer3
	lda #>(ScreenRAM+40*21+9)	; screen pos hi
	adc #$00			; add carry of screen pos lo
	sta pointer3+1
	lda temp_count_sum
	jsr Hex2Screencode		; calc screen code for a byte
	sty temp2
	ldy #$00
	sta (pointer3),y		; print to screen
	iny
	lda temp2
	sta (pointer3),y
	rts
; ----------------------------------------------------------------------------
; timer tests		; ********* added by Vossi **********
TimerTest:
	sei
	ldx #1				; "TIMERTESTS"
	jsr DrawMessage			; sub: draw message
	jsr InitSystemVectors		; sub: init system hardware vectors
	jsr InitCIAPointer		; sub: init cia pointer
	lda #SYSTEMBANK
	sta IndirectBank
	jsr eciairq
; test timers with IRQ
	ldy #$00
	lda #$00
	sta (cia+CRB),y			; disable timer B
	lda #$08
	sta (cia+CRA),y			; timer A one-shot
	sty timer_state			; clear fault counter
	ldx #$01			; irq-bit timer A
	jsr CheckIRQTimerA		; check timer A IRQ
	beq tok1
	dec timer_state			; dec fault counter
tok1:	jsr CheckIRQ
	bne tok2
	dec timer_state			; dec fault counter
tok2:	ldx #$01
	lda #$00
	sta (cia+CRA),y			; disable timer A
	jsr CheckIRQTimerA		; check timer a no IRQ
	beq tok3
	dec timer_state			; dec fault counter
tok3:	ldx #$01
	jsr CheckIRQ
	beq tok4
	dec timer_state			; dec fault counter
tok4:	lda (cia+CRA),y
	and #$fe
	sta (cia+CRA),y			; disable mtimer A
	lda #$08
	sta (cia+CRB),y			; timer B one shot
	ldx #$02			; irq-bit timer B
	jsr CheckIRQTimerB		; check timer B IRQ
	beq tok5
	dec timer_state			; dec fault counter
; test timer regs + load + load force
tok5:	lda (cia+CRB),y
	and #$fe
	sta (cia+CRB),y			; disable timer B	
	lda #$40			; test timer regs $40 times
	sta temp_dec_value
tireglp:lda #$00
	sta (cia+CRA),y
	sta (cia+CRB),y			; clear and stop all timers
	lda #$55
	sta (cia+TALO),y
	sta (cia+TAHI),y		; load with hi
	lda (cia+TALO),y
	cmp #$55			; check timer A lo reg with $55
	beq tok6
	dec timer_state			; dec fault counter
tok6:	lda (cia+TAHI),y
	cmp #$55			; check timer A hi reg with $55
	beq tok7
	dec timer_state			; dec fault counter
tok7:	lda #$aa
	sta (cia+TAHI),y
	sta (cia+TALO),y		; load with hi only!		
	lda (cia+TALO),y
	cmp #$55			; check timer A still $55 because load only with hi
	beq tok8
	dec timer_state			; dec fault counter
tok8:	lda (cia+TAHI),y
	cmp #$aa			; check timer A hi reg with $aa
	beq tok9
	lda #$ff
	sta timer_state			; set fault counter = $ff
tok9:	lda #$10
	sta (cia+CRA),y			; timer A force load to load lo
	lda (cia+TALO),y
	cmp #$aa			; check timer A lo reg with $aa
	beq tok10
	dec timer_state			; dec fault counter
tok10:	lda (cia+TAHI),y
	cmp #$aa			; check timer A hi reg is still $aa
	beq tok11
	dec timer_state			; dec fault counter
tok11:	lda #$55
	sta (cia+TBLO),y
	sta (cia+TBHI),y		; load with hi
	lda (cia+TBLO),y
	cmp #$55			; check timer B lo reg with $55
	beq tok12
	dec timer_state			; dec fault counter
tok12:	lda (cia+TBHI),y
	cmp #$55			; check timer B hi reg with $55
	beq tok13
	dec timer_state			; dec fault counter
tok13:	lda #$aa
	sta (cia+TBHI),y
	sta (cia+TBLO),y		; load with hi only!
	lda (cia+TBLO),y
	cmp #$55			; check timer B still $55 because load only with hi
	beq tok14
	dec timer_state			; dec fault counter
tok14:	lda (cia+TBHI),y
	cmp #$aa			; check timer B hi reg with $aa
	beq tok15
	dec timer_state			; dec fault counter
tok15:	lda #$10
	sta (cia+CRB),y			; timer B force load to load lo
	lda (cia+TBLO),y
	cmp #$aa			; check timer B hi reg is still $aa
	beq tok16
	dec timer_state			; dec fault counter
tok16:	lda (cia+TBHI),y
	cmp #$aa			; check timer B lo reg with $aa
	beq tok17
	dec timer_state			; dec fault counter
tok17:	lda #$09
	sta (cia+CRA),y			; timer A start with one shot
	lda #$cc
	sta (cia+TALO),y
	sta (cia+TAHI),y		; load $cccc in latch only because timer runs
	lda (cia+TALO),y
	cmp #$aa			; check value not load while timer A runs
	bmi tok18
	dec timer_state			; dec fault counter
tok18:	lda (cia+TAHI),y
	cmp #$aa			; check value not load while timer A runs
	beq tok19
	dec timer_state			; dec fault counter
tok19:	lda #$19
	ldx #$00
	sta (cia+CRA),y			; timer A start with one shot, force load
	txa
	sta (cia+CRA),y			; stop timer A after only a few cycles
	lda (cia+TALO),y
	and #$fe			; eleminate bit 0
	cmp #$c4			; check im timer A lo is now $c4 +/- 1 bit
	beq tok20
	dec timer_state			; dec fault counter
tok20:	lda (cia+TAHI),y
	cmp #$cc			; check timer hi has not changed
	beq tok21
	dec timer_state			; dec fault counter
tok21:	lda #$09
	sta (cia+CRB),y			; timer B start with one shot
	lda #$cc
	sta (cia+TBLO),y
	sta (cia+TBHI),y		; load $cccc in latch only because timer runs
	lda (cia+TBLO),y
	cmp #$aa			; check value not load while timer A runs
	bmi tok22
	dec timer_state			; dec fault counter
tok22:	lda (cia+TBHI),y
	cmp #$aa			; check value not load while timer A runs
	beq tok23
	dec timer_state			; dec fault counter
tok23:	lda #$19
	ldx #$00
	sta (cia+CRB),y			; timer B start with one shot, force load
	txa
	sta (cia+CRB),y			; stop timer B after only a few cycles
	lda (cia+TBLO),y
	and #$fe			; eleminate bit 0
	cmp #$c4			; check im timer A lo is now $c4 +/- 1 bit
	beq tok24
	dec timer_state			; dec fault counter
tok24:	lda (cia+TBHI),y
	cmp #$cc			; check timer hi has not changed
	beq tok25
	dec timer_state			; dec fault counter
tok25:	dec temp_dec_value
	bmi tok26
	jmp tireglp			; repeat $40 times
;
tok26:	lda #$00
	sta (cia+TAHI),y
	sta (cia+TBHI),y		; clear timers hi
	lda #$01
	sta (cia+TBLO),y		; load timer B lo = $01
	lda #$08
	sta (cia+TALO),y		; load timer A lo = $08
	lda #$51
	sta (cia+CRB),y			; start timer B, force load, counts underflow of timer A
	lda #$19
	sta (cia+CRA),y			; starts timer A, one shot, force load
	tax				; use $19 as counter
tdelay	dex
	bne tdelay			; delay
	txa				; $00 to a
	sta (cia+CRA),y			; stop + clear both timers
	sta (cia+CRB),y
	lda (cia+TBHI),y
	beq tok27			; timer B hi should be 0
	dec timer_state			; dec fault counter
tok27:	lda (cia+TBLO),y
	beq tok28			; timer B lo should be 0 because it counted underflow of timer A
	dec timer_state			; dec fault counter
tok28:	lda (cia+TAHI),y
	cmp #$00			; timer A hi should be 0
	beq tok29
	dec timer_state			; dec fault counter
tok29:	lda (cia+TALO),y
	cmp #$08			; timer A lo $08 because force load
	beq tok30
	dec timer_state			; dec fault counter
tok30:	lda timer_state			; dec fault counter
	beq tmrend			; skip if test ok
; timer fails
	lda #$3b
	sta pointer2
	lda #$d3
	sta pointer2+1			; set screen pointer to color RAM
	lda CodeBank
	jsr ColorFaultyChip		; color 6526 U02 - V=0 if already colored
	ldx #0				; text tmr
	jsr PrintChipText		; print text in 6526
tmrend:	rts
; ----------------------------------------------------------------------------
; check time A IRQ
CheckIRQTimerA:
	lda #$88			; set timer A to $8888
	sta (cia+TALO),y
	sta (cia+TAHI),y
	lda (cia+CRA),y
	ora #$01
	sta (cia+CRA),y			; start timer A
	jsr Delay
	bne CheckIRQ			; jump always
; check timer B IRQ
CheckIRQTimerB:
	lda #$88			; set timer B to $8888
	sta (cia+TBLO),y
	sta (cia+TBHI),y
	lda (cia+CRB),y
	ora #$01
	sta (cia+CRB),y
	jsr Delay
	bne CheckIRQ			; jump always
CheckIRQ:
	jsr cciairq			; clear IRQ reg
	txa
	sta temp_irq			; remember irq bit from x for timer
	sta (cia+ICR),y			; clear mask bit for timer IRQ
	ldx #$00			; reset counter for waiting for IRQ
	stx temp3			; reset hi counter for waiting
irqlp:	lda (cia+ICR),y			; load IRQ reg
	bne CheckTimerIRQok
	inx
	bne irqlp			; wait for IRQ
	inc temp3
	lda #$0f
	cmp temp3
	bpl irqlp			; wait for IRQ
	ldx temp3
	rts				; returns X=$0a if no IRQ occured
; ----------------------------------------------------------------------------
; check if correct IRQ + time
CheckTimerIRQok:
	and temp_irq			; isolate timer IRQ bit
	cmp temp_irq			; check if timer IRQ bit set?
	beq irqok			; skip if IRQ ok
	dec timer_state			; dec fault counter
irqok:	cpx #$db			; compare time
	beq timelok			; skip if IRQ ok
	dec timer_state			; dec fault counter
timelok:ldx temp3
	cpx #$0a
	beq timehok
	dec timer_state			; dec fault counter
timehok:rts
; ----------------------------------------------------------------------------
; Delay
Delay:
	lda #$05
	clc
-	sbc #$01
	bpl -
	rts
; ----------------------------------------------------------------------------
; enable all CIA interrupts
eciairq:ldy #$00
	lda #$7f			; clear all irq mask bits
	sta (cia+ICR),y
; clear CIA interrupt reg
cciairq:ldy #$00
	lda (cia+ICR),y			; clear irq reg
	rts
; ----------------------------------------------------------------------------
; return from
InterruptHandler:
	rti				; return from interrupt
; TOD tests		; ********* added by Vossi **********
TODTest:
	sei				; disable interrupts (ALARM test checks reg)
	ldx #2				; "TOD TESTS"
	jsr DrawMessage			; sub: draw message
	jsr InitCIAPointer		; init cia pointer
	jsr eciairq			; enable cia irq's
	ldy #$00
	sty tod_state			; init TOD state to 0 = ok
	lda #SYSTEMBANK
	sta IndirectBank		; systembank for cia
	sty time2_minutes		; init h,m,s vars to 0
	sty time2_seconds
	sty time2_10th
	sty time1_minutes
	sty time1_seconds
	sty time1_10th
	lda #$01
	sta time2_hours			; init hours vars to 1
	sta time1_hours
; test 10x 10th change in first second
chk10lp:lda time2_10th
	sta time1_10th			; copy 10th to time2
	jsr TODCheck			; check if TOD increases one 10th
	lda tod_state
	bne todfai1			; branch -> TOD failure
	lda time2_seconds
	beq chk10lp			; check all 10th
	jsr PlaySound			; play sound after 1 second
; test last 10th increases a second
	lda #$09
	sta time1_10th
chkslp: lda time2_seconds
	sta time1_seconds
	jsr TODCheck
	lda tod_state
	bne todfai1
	lda time2_minutes
	beq chkslp			; check all seconds
	jsr PlaySound
; test minutes change
	lda #$59
	sta time1_seconds
chkmlp:	lda time2_minutes
	sta time1_minutes
	jsr TODCheck
	lda tod_state
	bne todfai1			; branch -> TOD failure
	lda time2_hours
	cmp #$01
	beq chkmlp			; check all minutes
	jsr PlaySound
; test hours change
	lda #$59
	sta time1_minutes
chkhlp:	lda time2_hours
	sta time1_hours
	jsr TODCheck
	lda tod_state
	bne todfai1			; branch -> TOD failure
	lda time2_hours
	cmp #$01
	bne chkh12			; skip if not 1
	lda #$81
	sta time2_hours			; set pm
	bne chkhlp			; check all hours
chkh12:	cmp #$12
	bne chkhlp			; check all hours
	sta time1_hours
	jsr TODCheck
	lda tod_state			; load state
todfai1:bne todfail			; branch -> TOD failure
; TOD alarm test
	lda (cia+ICR),y			; clear cia irq reg
	lda #$7f
	sta (cia+ICR),y			; clear all irq mask bits
	lda #$80
	sta (cia+CRB),y			; set bit #7 - TOD ALARM
	lda time2_hours
	sta (cia+TODHR),y		; set ALARM
	lda time2_minutes
	sta (cia+TODMIN),y
	lda time2_seconds
	sta (cia+TODSEC),y
	lda time2_10th
	clc
	adc #$01
	sta (cia+TOD10),y		; set ALARM to time2 + one 10th
	sty tod_count1			; clear counter
	sty tod_count2
alarmlp:lda (cia+ICR),y
	bne chkalar			; irq -> test for ALARM irq bit #2
	dec tod_count1
	bne alarmlp			; wait for ALARM
	dec tod_count2
	bne alarmlp			; wait for ALARM
	beq todfail			; branch -> TOD failure
chkalar:cmp #$04			; test ALARM irq bit
	beq todend			; skip if tod ALARM OK	
; tod fails
todfail:lda #$3b
	sta pointer2
	lda #$d3
	sta pointer2+1			; set screen pointer to color RAM
	lda CodeBank
	jsr ColorFaultyChip		; color 6526 U02 - V=0 if already colored
	ldx #1				; text tod
	jsr PrintChipText		; print text in 6526
todend:	rts
; ----------------------------------------------------------------------------
; set TOD to time1 and set time2 = time1 + one 10th
; count for TOD change and compares to time2
TODCheck:
	sed				; decimal mode
	sty tod_count1			; clear counter
	sty tod_count2
	sty tod_count3
	lda time1_hours
	sta time2_hours
	sta (cia+TODHR),y		; set TOD starting with hours (halts TOD) to time1
	lda time1_minutes
	sta time2_minutes
	sta (cia+TODMIN),y
	lda time1_seconds
	sta time2_seconds
	sta (cia+TODSEC),y
	lda time1_10th
	sta (cia+TOD10),y		; set TOD 10th (starts TOD)
	clc
	adc #$01
	sta time2_10th			; set time2 = time1 + one 10th
	cmp #$10
	bne chktod			; skip if < 10
	sty time2_10th			; reset time2 10th
	clc
	lda time2_seconds
	adc #$01
	sta time2_seconds		; inc time2 seconds
	cmp #$60
	bne chktod			; skip if < 60
	sty time2_seconds		; reset time2 seconds
	clc
	lda time2_minutes
	adc #$01
	sta time2_minutes		; inc time2 minutes
	cmp #$60
	bne chktod			; skip if < 60
	sty time2_minutes		; reset time minutes
	clc
	lda time2_hours
	adc #$01
	sta time2_hours
	and #$1f			; isolate hours (without pm flag)
	cmp #$13
	bne chk12			; branch if time2 hours <>13
; set hours at 13 to 1 and toggles AM/PM
	lda time2_hours			; load time2 hours with pm flag
	and #$81			; at 13 reset hours to 1, preserve pm flag
	sta time2_hours
	bne togglpm			; jump always -> toggle am/pm
chk12:	cmp #$12
	beq togglpm			; if hours = 12 -> toggle pm flag
; toggles AM/PM back at 1
	cmp #$01
	bne chktod			; branch if hours > 1 and < 12
togglpm:lda #$80
	eor time2_hours			; toogle pm flag
	sta time2_hours
; count time for change of TOD to time1 init value
chktod:	lda (cia+TOD10),y
	cmp time1_10th
	bne todchg
	dec tod_count1			; dec counter if no change
	bne chktod
	dec tod_count2
	bne chktod
	dec tod_count3
	bne chktod
	beq todbad			; if TOD doesn't change in 999999 cycles -> bad
; compare new time to time2
todchg:	cmp time2_10th			; compare if TOD is now = time2
	bne todbad			; if not -> failure
	lda (cia+TODSEC),y
	cmp time2_seconds
	bne todbad			; ********** CMOS ERROR: TOD seconds still 1, but time2_seconds=2 **********
	lda (cia+TODMIN),y
	cmp time2_minutes
	bne todbad
	lda (cia+TODHR),y
	cmp time2_hours
	beq todok
todbad:	lda #$ff			; state = TOD bad
	sta tod_state
todok:	cld				; reset decimal flag
	rts
; ----------------------------------------------------------------------------
; test, copy code, switch to new bank
Test:
	ldx #3				; "TESTBANK"
	jsr DrawMessage			; sub: draw message
	lda #$ff
	sta test_mask			; test-mask - $ff = test all bits
	ldy last_rambank
	sty banks_counter		; counter for banks to test
	ldx CodeBank
	stx copy_source_bank		; source bank = actual codebank
	dex				; decrease for bank to test
	bpl tstnxbk			; skip if testbank is >= 0
	ldx last_rambank		; load last RAM bank if code is in bank 0
tstnxbk:stx copy_target_bank
; unused variable
;	stx $31				; remember target (test) bank $31
	ldx copy_target_bank
	stx IndirectBank		; set indirect bank = target bank
	jsr RAMTest			; sub: RAM Test - bank below code or last bank
	ldx copy_target_bank
	stx copy_source_bank		; source bank = last test bank
	dex				; decrease bank
	bpl tbnknt0			; skip if testbank is >= 0
	ldx last_rambank
tbnknt0:stx copy_target_bank		; store new target bank 
	dec banks_counter		; decrease banks to test counter
	bne tstnxbk			; test bank below
	ldy last_rambank
	sty banks_counter		; store last bank in banks counter
	ldx CodeBank
	dex				; decrease code bank
	bpl tchknbk			; skip if bank is >= 0
	ldx last_rambank		; load last RAM bank if code is in bank 0
tchknbk:lda bank_state,x		; check if RAM bank is OK = $00
	beq tstcpcd			; jump to code copy if new bank is OK
tscpnok:dex
	bpl notbk0d			; skip if new code bank is >= 0
	ldx last_rambank		; load last RAM bank if <0
notbk0d:dec banks_counter
	bne tchknbk			; check if next bank is OK as new code bank
	beq tstnocb			; skip code copy if new new OK code bank found
tstcpcd:stx temp5
	txa
	ldx #$00
	ldy #$00
	jsr SetCopyTarget		; sub: set copy target = new codebank, start=$0000
	lda CodeBank
	jsr SetCopySource		; sub: set copy source = actual codebank, start=$0000
	ldx #CODESIZE			; code size in pages to copy
	inx				; increase one page
	jsr CopyMemory			; copy code to new bank
	beq tstcpok			; branch if returns 0=ok
	ldx temp5
	bpl tscpnok			; try next bank if copy was not ok
tstcpok:ldy CodeBank			; remembers old code bank in Y
	ldx temp5
	stx CodeBank			; switch to new code bank
	nop
	nop
	nop
	nop
	sty copy_target_bank		; store old code bank as new test bank
	ldx copy_target_bank
	stx IndirectBank		; switch indirect bank to test bank
	jsr RAMTest			; test previous code bank after code copy 
tstnocb:jsr TestBank15			; test ram areas in bank 15
	rts
; ----------------------------------------------------------------------------
; Test RAMareas in bank 15
TestBank15:
	lda #SYSTEMBANK
	sta IndirectBank		; switch to bank 15
	ldy #$02
	ldx #$00
	lda #$08
	jsr RamTestBank15		; test $0002-$0800 = 6116 ZP
	lda CodeBank
	ldx #>ScreenRAM
	ldy #<ScreenRAM			; copy address = Screen RAM
	jsr SetCopyTarget		; set copy target = actual bank
	lda #SYSTEMBANK
	jsr SetCopySource		; set copy source = bank 15
	ldx #$08
	jsr CopyMemory			; copy 8 pages Screen+Color RAM
	lda #>ColorRAM
	ldy #<ScreenRAM
	ldx #>ScreenRAM
	jsr RamTestBank15		; test $d000-$d3ff = Screen RAMs
	lda #$0f
	sta test_mask			; set test mask = low nibble for color RAM
	lda #>VIC
	ldy #<ColorRAM
	ldx #>ColorRAM
	jsr RamTestBank15		; test $d400-$d7ff = Color RAM
	lda #SYSTEMBANK
	ldx #>ScreenRAM
	ldy #<ScreenRAM
	jsr SetCopyTarget		; copy target = $d000, bank 15
	lda CodeBank
	jsr SetCopySource		; copy source = actual code bank
	ldx #$08
	jsr CopyMemory			; copy 8 pages back to Screen+Color RAM
	rts
; ----------------------------------------------------------------------------
; RAM test
RAMTest:	
	lda #$00			; test start address = $0002
	tax
	ldy #$02			; test start address 
RamTestBank15:	
	sty start_low			; remember start address lowbyte (start with $02)
	stx start_high			; remember start address highbyte
	sta test_pages			; $00 = test all pages
	dey
	sty temp2			; remember last test page for downwards test end check
	lda #$00
	sta pointer1			; pointer1 lowbyte = $00
	sta pointer3
	lda #>(ScreenRAM+40+9)
	sta pointer3+1			; pointer3 = screen RAM
	lda IndirectBank
	sta temp_bank			; remeber sctual test bank	********* PATCHED *********
	ldy #SYSTEMBANK
	sty IndirectBank		; switch to bank 15
	jsr Nibble2Screencode		; convert bank number to screen code
	ldy #<(ScreenRAM+40+9)		; screen posotion test bank number	
	sta (pointer3),y		; write actual test bank number to screen
	lda temp_bank
	sta IndirectBank		; restore test bank			********* PATCHED *********
	cmp #$0f			; check if target = bank 15
	beq notbnkf			; skip if not bank 15
	jsr PlaySound			; play sound
notbnkf:ldy start_low			; start with Y = $02
	lda start_high
	sta pointer1+1			; start with page $00
; test1 with upcounting value
test1lp:tya				; Y as test-byte
	sta temp3			; remember test-byte
	sta (pointer1),y		; store test value to test-bank
	lda (pointer1),y
	eor temp3			; check test-byte
	and test_mask			; mask out bits to test - standard $ff = all tested
	beq +
	jsr TestError			; jump to test error
+	iny
	bne test1lp			; next byte
	inc pointer1+1			; increase highbyte
	lda pointer1+1
	cmp test_pages			; check if last test page
	bne test1lp			; next page
	jsr ResetStartAddress		; reset start address for next test
; test 2 with address highbyte
test2lp:tya
	sta temp3
	lda (pointer1),y		; check byte from last test again
	eor temp3
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	lda pointer1+1
	sta (pointer1),y
	lda (pointer1),y
	eor pointer1+1			; check with address highbyte
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	iny
	bne test2lp
	inc pointer1+1
	lda pointer1+1
	cmp test_pages
	bne test2lp
	jsr PlaySound			; play sound
	jsr ResetStartAddress		; reset start address for next test
; test 3 first byte with $55, second with $aa
	lda #$55
	sta temp3
	lda #$aa
	sta temp4
test3lp:lda (pointer1),y		; check byte from last test again
	eor pointer1+1
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	lda #$55
	sta (pointer1),y
	lda (pointer1),y
	eor temp3			; check with $55
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	iny				; next byte
	lda (pointer1),y		; check byte from last test again
	eor pointer1+1
	and test_mask
	beq +
	jsr TestError			; jump to test error
+ 	lda #$aa
	sta (pointer1),y
	lda (pointer1),y
	eor temp4			; check second byte with $aa
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	iny
	bne test3lp
	inc pointer1+1
	lda pointer1+1
	cmp test_pages
	bne test3lp
	jsr ResetStartAddress		; reset start address for next test
; test 4 first byte with $aa, second with $55
test4lp:lda (pointer1),y		; check byte from last test again
	eor temp3
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	lda #$aa
	sta (pointer1),y
	lda (pointer1),y
	eor temp4			; check with $aa
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	iny				; next byte
	lda (pointer1),y		; check byte from last test again
	eor temp4
	and test_mask
	beq l23c1
	jsr TestError			; jump to test error
l23c1:	lda #$55
	sta (pointer1),y
	lda (pointer1),y
	eor temp3			; check with $55
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	iny
	bne test4lp
	inc pointer1+1
	lda pointer1+1
	cmp test_pages
	bne test4lp
	jsr PlaySound			; play sound
	jsr ResetStartAddress		; reset start address for next test
; test 5 test with $5a
	ldx #$5a
	stx check
test5lp:lda (pointer1),y		; check byte from last test again
	eor temp4
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	txa
	sta (pointer1),y
	lda (pointer1),y
	eor check			; check with $5a
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	iny				; next byte
	lda (pointer1),y		; check byte from last test again
	eor temp3
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	txa
	sta (pointer1),y
	lda (pointer1),y
	eor check			; check with $5a
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	iny
	bne test5lp
	inc pointer1+1
	lda pointer1+1
	cmp test_pages
	bne test5lp
	jsr MaxStartAddress		; set address to maximum
; test 6 with $a5 downwards
	ldx #$5a
	stx temp3
	ldx #$a5
	stx temp4
test6lp:lda (pointer1),y		; check byte from last test again
	eor temp3
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	txa
	sta (pointer1),y
	lda (pointer1),y
	eor temp4			; check with $a5
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	dey				; next byte down
	cpy #$ff
	bne test6lp
	dec pointer1+1
	lda pointer1+1
	cmp start_high
	bne test6lp			; next page down
; check last page because downwards
tst6alp:lda (pointer1),y		; check byte from last test again
	eor temp3
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	txa
	sta (pointer1),y
	lda (pointer1),y
	eor temp4			; check with $a5
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	dey
	cpy temp2
	bne tst6alp			; next byte down
	jsr PlaySound			; play sound
	jsr MaxStartAddress		; set address to maximum
; test 7 with $5a downwards
	ldx #$5a
test7lp:lda (pointer1),y		; check byte from last test again
	eor temp4
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	txa
	sta (pointer1),y
	lda (pointer1),y
	eor temp3			; check with $5a
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	dey
	cpy #$ff
	bne test7lp			; next byte down		
	dec pointer1+1
	lda pointer1+1
	cmp start_high
	bne test7lp			; next page down
; check last page because downwards
tst7alp:lda (pointer1),y		; check byte from last test again
	eor temp4
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	txa
	sta (pointer1),y
	lda (pointer1),y
	eor temp3			; check with $5a
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	dey
	cpy temp2
	bne tst7alp
	jsr ResetStartAddress		; reset start address for next test
; test 8 with $ff
	ldx #$ff
	stx check
test8lp:lda (pointer1),y		; check byte from last test again
	eor temp3
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	txa
	sta (pointer1),y
	lda (pointer1),y
	eor check			; check with $ff
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	iny
	bne test8lp			; next byte
	inc pointer1+1
	lda pointer1+1
	cmp test_pages
	bne test8lp			; next page
	jsr PlaySound			; play sound
	jsr MaxStartAddress		; set address to maximum
; test 9 with $00 downwards
	ldx #$00
	stx temp3
	ldx #$ff
	stx temp4
test9lp:txa
	lda (pointer1),y		; check byte from last test again
	eor temp4
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	sta (pointer1),y
	lda (pointer1),y
	eor temp3			; check with $00
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	dey
	cpy #$ff
	bne test9lp			; next byte down
	dec pointer1+1
	lda pointer1+1
	cmp start_high
	bne test9lp			; next page down
; check last page because downwards
tst9alp:txa
	lda (pointer1),y		; check byte from last test again
	eor temp4
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	sta (pointer1),y
	lda (pointer1),y
	eor temp3			; check with $00
	and test_mask
	beq +
	jsr TestError			; jump to test error
+	dey
	cpy temp2
	bne tst9alp			; next byte down
	rts
; ----------------------------------------------------------------------------
; test error
TestError:
	clv				; clear overflow flag for bank 15 coloring check
	sta faulty_bits			; remember faulty bits
	stx storage1			; remember X
	sty storage2			; remember lowbyte
	ldx IndirectBank
	stx temp_bank			; remember defective test bank
	ldy temp_bank			; Y = defective test bank
	cpy #$0f			; check if bank 15
	beq errbnkf			; skip if bank 15
				; removed dey to show bank 0 fault ********* PATCHED *********
	lda #$ff
	sta bank_state_full,y		; store $ff to bank_state - ?? = defective bank
	ldy temp_bank			; load defective testbank
				; removed dey to show bank 0 fault ********* PATCHED *********
	lda BankScreenPosLo,y		; load screen-pointer to faulty bank from table
	sta pointer2
	lda BankScreenPosHi,y
	sta pointer2+1
	ldx #$08			; 8 bits to check
	stx temp5
errloop:lda faulty_bits			; load faulty bits - 1=faulty
	clc
	rol				; shift faulty bit in carry
	sta faulty_bits			; store <<faulty bits
	bcc erbitok			; skip if bit not faulty = 0
	jsr ColorFaultyChip		; color faulty RAM chip
erbitok:lda #$03
	clc
	adc pointer2
	sta pointer2			; add 3 to screen pointer for next chip
	bcc +
	inc pointer2+1
+	ldx temp5
	dex				; decrease bit counter
	stx temp5
	bne errloop			; check next bit
errfend:ldx temp_bank
	stx IndirectBank		; switch back to actual testbank
	ldx storage1			; restore start higbyte
	ldy storage2			; restore lowbyte
	lda #$00
	rts
; ----------------------------------------------------------------------------
; Test error in bank 15
errbnkf:lda pointer1+1
	and #$f0
	bne erfnt0x			; skip if error not in $0xxx
; faulty zp RAM U85
	lda #$44
	sta pointer2
	lda #$d3
	sta pointer2+1			; set screen pointer to faulty zeropage SRAM
	jsr ColorFaultyChip		; color 6116 U85 - returns V=0 if already colored
	bvc errfend			; back if already colored
erfnt0x:lda pointer1+1
	cmp #$d4
	bpl ercolor			; branch if >= $d4 color RAM fault
	lda faulty_bits
	and #$f0			; clear lower nibble of faulty bits
	beq ernibok
; faulty screen RAM high nibble
	lda #$a4
	sta pointer2
	lda #$d2
	sta pointer2+1			; set screen pointer to faulty screen RAM
	lda CodeBank
	jsr ColorFaultyChipCodeBank	; color 2114 U75 - high nibble
ernibok:lda faulty_bits
	and #$0f
	beq errfend			; back if no error in screen RAM low nibble
	lda #$a1
	sta pointer2
	lda #$d2
	sta pointer2+1			; set screen pointer to faulty screen RAM
	lda CodeBank
	jsr ColorFaultyChipCodeBank	; color 2114 U74 - low nibble
	bvc errfend			; back if already colored
; faulty color RAM
ercolor:lda #$41
	sta pointer2
	lda #$d3
	sta pointer2+1			; set screen pointer to color RAM
	lda CodeBank
	jsr ColorFaultyChipCodeBank	; color 2114 U24 - V=0 if already colored
	bvc errfend			; back if already colored
; ----------------------------------------------------------------------------
; color the faulty chip		
ColorFaultyChip:
	lda #SYSTEMBANK
ColorFaultyChipCodeBank:		
	sta IndirectBank		; switch to bank 15
	ldy #$00
	lda (pointer2),y		; load char from screen
	bmi coldone			; return if chip already colored
	lda pointer2					
	sta pointer3			; copy screen pointer to pointer 3
	lda pointer2+1
	sta pointer3+1
	ldx #$03			; 3 chars high
colnxty:ldy #$01			; 2 chars wide
colnxtx:lda #REVSPACE
	sta (pointer3),y		; write reverse space to screen
	dey
	bpl colnxtx			; next char at the left
	lda #$04
	clc
	adc pointer3+1			; add $04 to highbyte for color RAM
	sta pointer3+1			; ADC set V-flag if $D7 / bank 15
	ldy #$01
colcolx:lda #YELLOW
	sta (pointer3),y		; write color
	dey
	bpl colcolx			; next char at the left
	lda pointer3+1
	sec
	sbc #$04			; sub $04 to get back to screen RAM
	sta pointer3+1
	lda #$28
	clc
	adc pointer3			; add 40 for next line
	sta pointer3
	bcc +
	inc pointer3+1
+	dex
	bne colnxty			; next line
coldone:rts				; returns V = 1 if bank 15 chip was colored!
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
	stx counter			; save pages to copy to counter $35, temp $43
	stx temp1
	ldx IndirectBank		; save indirect bank to temp_bank
	stx temp_bank
	ldy #$00			; start low byte Y = $00
	cpy copy_target+1
	bne +				; skip if target higbyte is $00
	ldy #$02			; start low byte = $02 if page 0
+	sty temp6			; remember start low byte
-	ldx copy_source_bank
	stx IndirectBank		; set source bank
	lda (copy_source),y		; load source byte
	ldx copy_target_bank		; set target bank
	stx IndirectBank
	sta (copy_target),y		; save byte to target
	iny
	bne -				; copy next byte
	inc copy_source+1		; increase high bytes
	inc copy_target+1
	dec counter			; decrease page counter
	bne -				; copy next page

	lda temp1			; restore page to counter
	sta counter
	lda #$00			; clear $47
	sta error
	ldy temp6			; load start low byte
	lda temp3			; check if temp4 = $00
	ora temp4
	and temp4
	bne +				; skip if $45 not $00
	ldy #$48			; start at low byte $48
+	lda temp3
	sta copy_source+1		; source high byte = $44
	lda temp4
	sta copy_target+1		; target high byte = $45
-	ldx copy_source_bank		; set source bank
	stx IndirectBank
	lda (copy_source),y		; load source byte
	sta check			; save to check
	ldx copy_target_bank		; set target bank
	stx IndirectBank
	lda (copy_target),y		; load target byte
	eor check			; A=0 if source = target
	ora error			; add state to error variable  
	sta error			; save new state
	iny
	bne -				; check next byte
	inc copy_source+1		; increase high bytes
	inc copy_target+1
	dec counter			; decrease page counter
	bne -				; check next page
	ldx temp_bank			; restore indirect bank
	stx IndirectBank
	lda error			; return error state 0=ok, 1=error
	rts
; ----------------------------------------------------------------------------
; Store copy source from A=bank, X=high, Y=low
SetCopySource:	
	sta copy_source_bank
	stx copy_source+1
	stx temp3
	sty copy_source
	rts
; ----------------------------------------------------------------------------
; Store copy target from A=bank, X=high, Y=low
SetCopyTarget:	
	sta copy_target_bank
	stx copy_target+1
	stx temp4
	sty copy_target
	rts
; ----------------------------------------------------------------------------
; Calc screencode digits for byte in A to AY
Hex2Screencode:	
	pha				; remember value on stack
	jsr Nibble2Screencode		; calc low nibble
	tay				; remember lower digit in Y
	pla				; restore value
UpperNibble2Screencode:
	lsr				; upper nibble -> lower nibble
	lsr
	lsr
	lsr
Nibble2Screencode:
	and #$0f			; isolate low nibble
	cmp #$0a
	bmi scdec			; skip if 0-9
	sec
	sbc #$09			; calc screencode A-F -> 01-06
	bne scend			; jump always
scdec:	ora #$30			; calc screencode 0-9 -> 30-39
scend:	rts
; ----------------------------------------------------------------------------
; Set exterior color
SetExteriorColor:
	inc ext_color			; increase exterior color
	lda #>VIC
	sta color_pointer+1
	lda #EXTCOL			; exterior color
	sta color_pointer
	lda ext_color
	ldy #$00
	sta (color_pointer),y
	rts
; ----------------------------------------------------------------------------
; Draw Message - X = message number
DrawMessage:
	cpx #0
	beq +				; skip for message 0
	lda #$00
-	clc
	adc #11				; add 11 chars for next message
	dex
	bne -
	tax
+	lda IndirectBank
	sta temp_bank			; remember target bank
	lda #SYSTEMBANK
	sta IndirectBank		; indirect bank = bank 15
	lda #<(ScreenRAM+40)
	sta pointer1
	lda #>(ScreenRAM+40)
	sta pointer1+1
	ldy #$00
-	lda Messages,x
	sta (pointer1),y
	inx
	iny
	cpy #11
	bne -
	lda temp_bank
	sta IndirectBank		; restore target bank
	rts
; ----------------------------------------------------------------------------
; print text number x in 6526 chip
PrintChipText:
	txa				; message number = text column
	clc
	adc #<(ScreenRAM+40*20+27)	; add screen position lo
	sta pointer3
	lda #>(ScreenRAM+40*20+27)	; screen pos hi (already in last screen page)
	sta pointer3+1
	cpx #0
	beq +				; skip for text tmr
	ldx #3				; message tod
+	ldy #0				; line 0
	lda ChipTexts,x
	ora #$80			; reverse
	sta (pointer3),y		; print to screen
	ldy #40				; next 1
	inx
	lda ChipTexts,x
	ora #$80			; reverse
	sta (pointer3),y		; print to screen
	ldy #80				; next 2
	inx
	lda ChipTexts,x
	ora #$80			; reverse
	sta (pointer3),y		; print to screen
	rts
; ----------------------------------------------------------------------------
; copies CIA pointer to ZP
InitCIAPointer:
	lda #<cia			; cia pointer in ZP
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<CIARegs			; XA = CIATable
	ldx #>CIARegs
	ldy #$1f			; bytes to copy = $00-$1f
	jsr CopyPointer			; sub: copy register pointer
	rts
; ----------------------------------------------------------------------------
; unused - copies Triport2 pointer to ZP
	lda #<tpi2	
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<TPI2Regs
	ldx #>TPI2Regs
	ldy #$0f
	jsr CopyPointer
	rts
; ----------------------------------------------------------------------------
; unused - copies Triport1 pointer to ZP
	lda #<tpi1	
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<TPI1Regs
	ldx #>TPI1Regs
	ldy #$0f
	jsr CopyPointer
	rts
; ----------------------------------------------------------------------------
; unused - copies ACIA pointer to ZP
	lda #<acia	
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<ACIARegs
	ldx #>ACIARegs
	ldy #$07
	jsr CopyPointer
	rts
; ----------------------------------------------------------------------------
; copy sid-pointer-table to zeropage for indirect access
InitSIDPointer:
	lda #<sid			; sid pointer in ZP
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<SIDRegs			; XA = SIDTable
	ldx #>SIDRegs
	ldy #$39			; bytes to copy = $00-$39
	jsr CopyPointer			; sub: copy table
	rts
; ----------------------------------------------------------------------------
; init system vectors for timer test
InitSystemVectors:
	lda #<nmi_pointer
	sta HW_NMI
	lda #$00
	sta HW_NMI+1
	lda #<reset_pointer
	sta HW_RESET
	lda #$00
	sta HW_RESET+1
	lda #<irq_pointer
	sta HW_IRQ 
	lda #$00
	sta HW_IRQ+1
	lda #<InterruptHandler
	sta nmi_pointer
	sta reset_pointer
	sta irq_pointer
	lda #>InterruptHandler
	sta nmi_pointer+1
	sta reset_pointer+1
	sta irq_pointer+1
	rts
; ----------------------------------------------------------------------------
; copy $00-Y bytes in codebank from XA to pointer1
CopyPointer:
	sta pointer2			; store XA in pointer2
	stx pointer2+1
	lda CodeBank			; load actual code bank
	sta IndirectBank		; set indirect = code bank
-	lda (pointer2),y		; copy byte
	sta (pointer1),y
	dey
	bpl -				; next byte 
	rts
; ************************************* ZONE TABLES ***********************************************
!zone tables
Messages:
	!scr "checksums  "
	!scr "timertest  "
	!scr "tod tests  "
	!scr "testbank   "

ChipTexts:
	!scr "tmr"
	!scr "tod"

BankScreenPosLo:
	!byte $89, $a9, $49, $e9 ; ************ patched last byte to first

BankScreenPosHi:
	!byte $d2, $d0, $d1, $d1 ; *********** patched last byte to first

;282a
!scr " * *  BAD PROGRAM CHECKSUM  * * "

;284a
; unused - VIC pointer
VICRegs:
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
SIDRegs:
	!byte $00, $da, $01, $da, $02, $da, $03, $da
	!byte $04, $da, $05, $da, $06, $da, $07, $da
	!byte $08, $da, $09, $da, $0a, $da, $0b, $da
	!byte $0c, $da, $0d, $da, $0e, $da, $0f, $da
	!byte $10, $da, $11, $da, $12, $da, $13, $da
	!byte $14, $da, $15, $da, $16, $da, $17, $da
	!byte $18, $da, $19, $da, $1a, $da, $1b, $da
	!byte $1c, $da
; CIA Regs copied to $92
CIARegs:
	!byte $00, $dc, $01, $dc, $02, $dc, $03, $dc
	!byte $04, $dc, $05, $dc, $06, $dc, $07, $dc
	!byte $08, $dc, $09, $dc, $0a, $dc, $0b, $dc
	!byte $0c, $dc, $0d, $dc, $0e, $dc, $0f, $dc
; unused - ACIA pointer copied to $92
ACIARegs:
	!byte $00, $dd, $01, $dd, $02, $dd, $03, $dd
; unused - Triport1 pointer copied to $52
TPI1Regs:
	!byte $00, $de, $01, $de, $02, $de, $03, $de
	!byte $04, $de, $05, $de, $06, $de, $07, $de
; unused - Triport2 pointer copied to $62
TPI2Regs:
	!byte $00, $df, $01, $df, $02, $df, $03, $df
	!byte $04, $df, $05, $df, $06, $df, $07, $df
; ************************************* ZONE SCREENDATA *******************************************
!zone screendata
*= $3000
ScreenData:
	!scr "rambanks   ",$65,"pet ii diagnostic",$67,"  cycles  "
	!scr "           ",$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,"          "
	!scr "                                        "
	!scr "         d7 d6 d5 d4 d3 d2 d1 d0        "

	!scr "         ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50,"  "
	!scr "  bank 1 ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a,"  "
	!scr "         ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a,"  "
	!scr "         33 34 35 36 37 38 39 40        "

	!scr "         ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50,"  "
	!scr "  bank 2 ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a,"  "
	!scr "         ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a,"  "
	!scr "         41 42 43 44 45 46 47 48        "

	!scr "         ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50,"  "
	!scr "  bank 3 ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a,"  "
	!scr "         ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a,"  "
	!scr "         58 59 60 61 62 63 64 65        "
;	!scr "         58 59 60 61 62 63 64 65 76 82  "

	!scr "         ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50,"  "
	!scr "  bank 0 ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a,"  "
	!scr "         ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a,"  "
	!scr "         73 72 71 70 69 68 67 66 74 75  "

	!scr "         ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50," ",$4f,$50,"  "
	!scr "         ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a," ",$74,$6a,"  "
	!scr "p500test ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a," ",$4c,$7a,"  "
	!scr "vers 1.3 82 83 84          02    24 85  "
;	!scr "vers 1.1 83 84 04 19 20 82 02    24 85  "				; original
	!scr "vossi'23                                "

	!byte $00, $20, $20, $20, $20, $20, $20, $20				; last 24 bytes behind screenRAM
	!byte $20, $20, $20, $20, $20, $20, $20, $20
	!byte $20, $20, $20, $20, $20, $20, $20, $20

!ifdef ROM{
*= $3fff
	!byte $00
}

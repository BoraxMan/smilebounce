	;-----------------------------------------------------
	;
	; Bounce a smiley face around the screen
	;
	; There is small game element to this.  Use 'W', 'A', 'S', 'D'
	; to move a pointer around the screen.  Try and get the pointer on the
	; face for a screen flashing effect.
	;
	; Vic 20 assembly demo by
	; Dennis Katsonis, Sep 2020
	;
	; This is my first 6502 assembly program.  I have a Vic 20
	; but it doens't work.  If I can restore it, I'll be able to
	; run this on native hardware!
	;
	; To be run on an unexpanded Vic20 (i.e., no additional memory)
	; It may not work at all with memory expansion.
	;
	; Compile using ca65
	; cl65 -t vic20 -C vic20.cfg heartvic.asm
	;
	; May not display correctly on NTSC models.
	;
	; This is public domain code.  
	;-----------------------------------------------------
	
	; The memory layout of this program is as follows
	; $1000 - $17FF : BASIC
	; $1800 - $1BFF : Screen map (We only need 750 bytes)
	; $1C00 - $1FFF : Custom Characters (We only use the first few bytes)

	bsout=$FFD2
	offset=$FB		;  Two bytes for offset of SCREENMAP
	coloffset = $45		; Current variable name ZP location.  Use this space
	xpos = $FD
	ypos = $FE
	deltax = $F9 		; RS-232 tx pointer ZP location.  We aren't using this.
	deltay = $F8		; We store delta x and y here.
	sndcnt = $FA
	scrolltimer = $B7 	; # of characters in filename in ZP location.
	; We aren't using this so store scrolltimer
	currentchar = $B8 	; Current logical file in ZP location.
	; We aren't using this so store current char for scrolling text
	currentscrollcol = $B9 	; Current secondary address in ZP location
	; Store scrolling colour text here.
	blocker_x = $BA
	blocker_y = $BB
	blockerchar = $BC

	
	SCREENMAP=36869
	HORIZOFFSET=36864
	VERTOFFSET=$9001	; 36865
	COLSIZE=$9002 	;36866
	ROWSIZE=36867
	ROWS=30
	COLUMNS=25
	VOLUME=36878
	SPEAKER2=36875
	SPEAKER3=36874
	SCREENMEM=$1800
	COLOURMEM=$9400
	SCROLLDELAY=5


	
	PRA  =  $dc00            ; CIA#1 (Port Register A)
	PRB  =  $dc01            ; CIA#1 (Port Register B)

	DDRA =  $dc02            ; CIA#1 (Data Direction Register A)
	DDRB =  $dc03            ; CIA#1 (Data Direction Register B)

	SCROLLMESSAGELENGTH=57
	O=SCREENMEM+((ROWS-1)*COLUMNS)-1 ; Offsets for grass
	P=COLOURMEM+((ROWS-1)*COLUMNS)-1

	lda #SCROLLDELAY
	sta scrolltimer
	sta blocker_y
	sta blocker_x
	sta currentscrollcol	; Set colour to same as scroll timer (5)
	lda #2
	sta xpos
	sta ypos		; Set starting coordinates to 1,1
	lda #1
	sta deltax
	sta deltay
	lda #0
	sta sndcnt
	lda $52 		; Save end of basic memory
	sta old52
	lda $56
	sta old56
	lda SCREENMAP
	sta oldSCREENMAP ; Save current pointer to character set and other VIC 

	lda #24			; Set end of basic to $1800
	; This gives us only 2K of space to work in!  Although very limited by
	; todays standards, its more than enough for this program.
	sta $52
	lda #24		      ; Set end of basic to $1800.  This is where our characters
	; will be stored
	sta $56		; Lower end of basic memory to make room
	; for our custom character set.
	
	lda COLSIZE
	sta oldCOLSIZE
	lda #COLUMNS		; This will clear the 7th bit, moving
	sta COLSIZE		; the location of the screen map to 7168 ($1c00)
	; It will also move the colour map to 37888 ($9400)
	lda ROWSIZE
	sta oldROWSIZE
	and #129
	ora #(ROWS*2)
	sta ROWSIZE

	lda HORIZOFFSET			; save screen information
	sta oldHORIZOFFSET
	clc
	sbc #3			; Shift screen to the left
	sta HORIZOFFSET
	
	lda VERTOFFSET
	sta oldVERTOFFSET
	clc
	sbc #12			; Shift screen up
	sta VERTOFFSET

	lda #(2*16)	; Set auxillary colour.  Multiply by 16 because it is bits 7-4
	; we need to set.

	sta VOLUME		; Auxillary colour is stored here, it occupies
	; the top 4 bits of volume.
	
	ldx #0
copychars:
	lda smiley,x
	sta $1C00,x
	inx
	cpx #(8*12)		; Copy first 10 characters
	bne copychars

	
	lda #239		; 254 is 240 AND 14
	; The 14 refers to the lower three bits, which places the character map at 6144
	; 240 refers to bits 4-7, which with bit 7 of 36866 cleared, places the screen map
	; at 7168
	; This gives the screen map enough space for the extended screen size.
	; Refer to technical documentation for more info.
	sta SCREENMAP		; :Point VIC to our own character set
 	lda #14			; Black background, blue border
	sta $900F 		; Set background/foreground

	jsr colourtoprow
	
	ldx #0
	lda #%1100
colloop1:
	sta $9419,x
	sta $9500,x
	sta $9600,x
	dex
	bne colloop1  		; Loop over color map and set colours.
	; We will only worry about the first 768 bytes.
	
	ldx #0
	lda #($20+128)			; 09 is the blank space in our character set
loop1:  sta $1800,x
	sta $1900,x
	sta $1A00,x
	dex
	bne loop1 		; Loop over character map and set to space
	; We will only worry about the first 768 bytes.
	
	ldx #25
drawgrass: 			; Draw the characters which depict grass.
	lda #1
	sta O,x
	dex
	lda #2
	sta O,x
	dex
	lda #6
	sta O,x
	dex
	lda #7
	sta O,x
	dex
	lda #8
	sta O,x
	dex
	bne drawgrass

	ldx #30
	lda #5
colorgrass:
	sta P,x
	dex
	bne colorgrass

	
colorloop:
	ldx xpos
	ldy ypos
	
	jsr writemsg

	cpx #(COLUMNS-2)			;Last Column
	beq flipx
	cpx #0
	beq flipx
xdone:
	cpy #(ROWS-3)			;Second last row (grass is on the last row
	beq flipy
	cpy #1
	beq flipy
ydone:
	jsr adjustxy
	stx xpos
	sty ypos

	jsr drawstars

	ldx ypos
	jsr plotchar
	jsr plotcolour
	jsr checkkeyboard

	jsr drawface
	lda #11
	sta blockerchar
	jsr drawblocker
	jsr wait
	ldx ypos
	jsr plotchar
	jsr plotcolour
	jsr eraseface
	lda #($20+128)
	sta blockerchar
	jsr drawblocker
	jsr checksnd
	jsr $ffe1		
	bne colorloop
	jmp end
	
	;==============================================================
	; This will negate DELTAX to switch the X direction of the face.
	;
	; Clobbers A
	;===============================================================
flipx:
	lda deltax
	eor #$fe		; Negate all the bits except LSB
	sta deltax		;   Effectively alternating between the values
	; of 1 and 254 
	;  If we add 254, it is effectively the same as subtracting 1
	jsr beep
	jsr colourtoprow
	jmp xdone

	;==============================================================
	; This will negate DELTAY to switch the X direction of the face.
	;
	; Clobbers A
	;===============================================================
flipy:
	lda deltay
	eor #$fe
	sta deltay
	jsr beep
	jmp ydone



	;==================================================================
	; Calculates the offset in the COLOURMAP
	; Only to the row, not the column.
	;
	; X = Row
	;==================================================================
plotcolour:
	pha
	txa
	pha
	lda #00
	sta coloffset
	lda #$94
	sta coloffset+1
	beq cendplot 		; Add nothing if row 0
cplotitl:
	clc
	lda coloffset
	adc #COLUMNS
	sta coloffset
	lda coloffset+1
	adc #0
	sta coloffset+1
	dex
	bne cplotitl
cendplot:
	pla
	tax
	pla
	rts


	;=================================================================
	; This calculates the offset of the LINE that we will
	; draw on.
	;
	; X = Row
	;==================================================================
plotchar:
	pha
	txa
	pha
	lda #00
	sta offset
	lda #$18
	sta offset+1
	beq endplot 		; Add nothing if row 0
plotitl:
	clc
	lda offset
	adc #COLUMNS
	sta offset
	lda offset+1
	adc #0
	sta offset+1
	dex
	bne plotitl
endplot:
	pla
	tax
	pla
	rts

	;==================================================================
	; Adjust X and Y positions
	;==================================================================
adjustxy:	
	clc
	txa
	adc deltax		; Add the 'delta x', which would either
	; increment or decrement this by 1.
	tax
	
	clc
	tya
	adc deltay
	tay
	rts

	;=================================================================
	;Check if a sound is playing.  If so, lower the volume
	;=================================================================
checksnd:
	lda sndcnt
	cmp #0
	beq skipcheck
	lda VOLUME
	sbc #3
	sta VOLUME
	dec sndcnt
	bne skipcheck
	lda #0
	sta SPEAKER2
	STA SPEAKER3
skipcheck:
	rts

	;==================================================================
	;Short delay.  4/60th of a second to be exact;
	;==================================================================
wait:
	lda #0
	sta 162			;Store 0 in the timer register
waitl:
	lda 162			;Load it.  It will increment by one every jiffy			; (1/60th of a second)
	cmp #4			;  Four jiffies passed?
	bne waitl
	rts

	;====================================================================
	; Start a sound
	; checksnd will turn this off eventually.  This just starts the sound
	;
	; Clobbers A
	;=====================================================================
beep:
	lda VOLUME
	ora #15
	sta VOLUME
	lda #230
	sta SPEAKER2
	lda #200
	sta SPEAKER3
	lda #5			; This is how many frames before the sound fades out
	sta sndcnt
	rts

	;======================================================================
	; Routine to draw the face
	; Y = X column
	;
	; Clobbers A and Y
	;======================================================================
drawface:
	clc
	ldy xpos
	lda #0			; Load the PETSCII for a heart.
	sta (offset),y		; Save the heart at offset + y (which actually holds
	lda #%1100
	sta (coloffset),y
	; the X position).
	tya
	adc #1
	tay
	lda #3
	sta (offset),y
	lda #%1100
	sta (coloffset),y
	tya
	adc #24
	tay
	lda #%1100
	sta (coloffset),y
	lda #4
	sta (offset),y
	tya
	adc #1
	tay
	lda #%1100
	sta (coloffset),y
	lda #5
	sta (offset),y
	rts

	;======================================================================
	;Routine to erase the face
	;======================================================================
eraseface:
	clc
	lda #($20+128)
	ldy xpos
	sta (offset),y		; Save the heart at offset + y (which actually holds
	; the X position).
	tya
	adc #1
	tay
	lda #($20+128)
	sta (offset),y
	tya
	adc #24
	tay
	lda #($20+128)
	sta (offset),y
	tya
	adc #1
	tay
	lda #($20+128)
	sta (offset),y
	rts
	
	;===================================================================
	; Print scrolling message at the top of the screen.
	;
	; Clobbers A
	;===================================================================
writemsg:
	dec scrolltimer
	bne writeend
	lda #SCROLLDELAY
	sta scrolltimer
	
	lda textoff
	pha			; Store text offset

	tya
	pha
	txa
	pha

	ldx #0
writemsg2:
	txa
	pha			; Save X value (count, screen offset)
	ldy #>msg
	ldx #<msg
	stx offset
	sty offset+1
	lda textoff
	clc
	adc #1
	sta textoff
	tay
	lda (offset),y		; Load character
	sta currentchar
	cpy #SCROLLMESSAGELENGTH
	bne skip4
	lda #0
	sta textoff
skip4:
	lda #$18
	sta offset+1
	lda #0
	sta offset		; Restore screen offset
	pla
	tax			; and X value
	tay
	lda currentchar
	sta (offset),y		; And store
	inx
	cpx #24
	bne writemsg2
	pla
	tax
	pla
	tay

	pla			; Get original text offset
	clc
	adc #1			; Increment
	cmp #SCROLLMESSAGELENGTH			; Are we at 18?
	bne writeendp
	lda #0
writeendp:
	sta textoff
writeend:
	rts

	;===================================================================
	; This sets the colourmap of the top row of the display,
	; for the scrolling text
	;===================================================================
colourtoprow:
	txa
	pha
	ldx #24
toprow1:
	lda currentscrollcol
	sta $9400,x
	dex
	bne toprow1  		; Loop over color map and set to purple.
	; We will only worry about the first 768 bytes.
	dec currentscrollcol
	bne colourend
	lda #7
	sta currentscrollcol
colourend:
	pla
	tax
	rts

	;==================================================================
	; This draws a "target" cirle in the current user selected position
	;==================================================================

drawblocker:
	tya
	pha
	txa
	pha
	ldx blocker_y
	jsr plotchar
	ldy blocker_x
	lda (offset),y
	cmp #0
	beq hit
	cmp #3
	beq hit
	cmp #4
	beq hit
	cmp #5
	beq hit
	jmp missed
hit:
	;  A hit!
	; Did we display a boom last time?
	lda firepressed_prev
	bne skipboom
	ldx #8
boom:
	txa
	sta $900F
	jsr wait
	dex
	bne boom
	lda #14			; Black background, blue border
	sta $900F 		; Set background/foreground
	lda #1
	sta firepressed		; We have hit!
	jmp dbend

skipboom:
	
missed:
	lda firepressed
	sta firepressed_prev
	lda #0
	sta firepressed

dbend:
	lda blockerchar
	sta (offset),y
	jsr plotcolour
	cmp #($20+128)
	bne yell
	lda #%1100
	jmp yell2
yell:
	lda #%111		; Yellow
yell2:
	sta (coloffset),y
	pla
	tax
	pla
	tay
	rts

	;==================================================================
	; Check keyboard input
	;==================================================================

checkkeyboard:
	pha
	lda $c5			; Load current key pressed from ZP
	cmp #41			; an S?
	beq down
	cmp #9			; W?
	beq up
	cmp #17			; Q?
	beq left
	cmp #18			; E?
	beq right
	jmp checkend
right:
	lda blocker_x
	cmp #(COLUMNS-1)
	beq checkend
	inc blocker_x
	jmp checkend
left:
	lda blocker_x
	cmp #0
	beq checkend
	dec blocker_x
	jmp checkend
down:
	lda blocker_y
	cmp #(ROWS-2)
	beq checkend
	inc blocker_y
	jmp checkend
fire:
	lda #1
	sta firepressed
	jmp checkend
up:
	lda blocker_y
	cmp #1
	beq checkend
	dec blocker_y
checkend:
	pla
	rts

	
	;==================================================================
	; End routine
	;==================================================================
	
end:
	; All this code to restore the VIC 20 isn't really that necessary,
	; as people were used to switching their Vic off and on after loading a program.
	; But if we can avoid it, we should.  I always wanted programs to be able to
	; be exited, so we will offer this here.
	
	lda oldSCREENMAP	; Restore character set and other VIC values to restore
	sta SCREENMAP		; screen size and position.
	lda oldCOLSIZE
	sta COLSIZE
	lda oldROWSIZE
	sta ROWSIZE

	lda oldHORIZOFFSET	; Restore screen offsets.
	sta HORIZOFFSET
	lda oldVERTOFFSET
	sta VERTOFFSET

	lda old52
	sta $52
	lda old56
	sta $56		; Restore end of basic memory

	; Restore border and screen colour
	lda #27
	sta $900F
	lda #6 			; Restore text color
	sta 646			; 

	lda #0			; Turn sounds off, in case a sound is still playing
	sta SPEAKER2
	sta SPEAKER3
	lda #147
	jsr $FFD2		; Clear screen, as there may be junk
	rts
	
drawstars:
	;==================================================================
	; Draw Stars
	;==================================================================
	pha
	txa
	pha
	tya
	pha
	lda scrolltimer
	and #%11
	bne starsame
	lda starchar
	eor #%11		; Will flip between 9 and 10
	sta starchar
starsame:
	ldx #0
starloop:
	txa			; Save X count
	pha
	lda stars,x		; Load stars array
	tax			; And tranfer to Y
	jsr plotchar
	jsr plotcolour		; To calculate offset.  This will be the Y position
	pla
	tax			; Restore x count
	inx			; Increase to read X pos
	lda stars,x		; Load
	tay			; Transfer to Y
	lda starchar		; Load character to use
	sta (offset),y		; and store at offset
	lda #1
	sta (coloffset),y
	inx			; Go to next value
	cpx #(22*2)			; If 6 (3 lots of stars), skip
	bne starloop
	
	pla
	tay
	pla
	tax
	pla

	rts
	
;============================================================================
; DATA BEGINS HERE
;============================================================================

old52: .byte 0
old56: .byte 0
oldSCREENMAP: .byte 0
oldHORIZOFFSET: .byte 0
oldVERTOFFSET: .byte 0
oldCOLSIZE: .byte 0
oldROWSIZE: .byte 0
textoff: .byte 0
firepressed: .byte 0
firepressed_prev: .byte 0
starchar: .byte 9
stars: .byte 20,20,13,22,14,4,15,14,15,16,15,23,15,5,15,6,16,22,16,3,17,20,22,11,23,24,23,3,26,24,26,6,4,3,6,19,7,14,17,14,8,18,9,3
msg: .byte $20+128,2+128,$0f+128,$15+128,$0e+128,03+128,09+128,$0e+128,07+128,$20+128,2+128,01+128,$0c+128,$0c+128,$20+128,04+128,05+128,$0d+128,$0f+128,$20+128,$20+128,$20+128,$20+128,02+128,$19+128,$20+128,$04+128,$05+128,$0e+128,$0e+128,$09+128,$13+128,$20+128
msg_cont:	.byte $0b+128,$01+128,$14+128,$13+128,$0f+128,$0e+128,$09+128,$13+128,$20+128,$20+128,$20+128,$20+128,$13+128,$05+128,$10+128,$20+128,$32+128,$30+128,$32+128,$30+128,$20+128,$20+128,$20+128,$20+128,$20+128
	;This is the message "BOUNCING BALL DEMO  BY DENNIS KATSONIS SEP 2020" in PETSCII
	; 128 is added to each value to select the "REVERSE" character set.  Because
	; we have shifted the pointer to the character ram forwards, selecing the reverse
	; characters actually ends up selecting the regular characters in ROM.
	;
	; There might by a better way to do this, I'll sort it out later.
	
smiley: .INCBIN "characters-charset.bin",0,(12*8)

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
	; $1000 - $17FF : BASIC (Just enough to squeeze in this program)
	; $1800 - $1BFF : Screen map (We only need 750 bytes)
	; $1C00 - $1FFF : Custom Characters (We only use the first few bytes,
	; so the space after this could be used as storage.)

	bsout=$FFD2
	offset=$FB		; Two bytes for offset of SCREENMAP
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

	; Zero Page pointers to the music.  Because the total number of notes for each
	; song is greater than 255, we need to be able to dynamically
	; point between song 1 and 2, otherwise if we just point to song 1 and go,
	; X will overflow while referencing the second song.
	
	song_notes_treble = $0A ; Load/Verify, basic stuff we aren't use
	song_notes_bass = $0C	 ; Input buffer pointer for BASIC we aren't using
	
	HORIZOFFSET=$9000
	VERTOFFSET=$9001	; 36865
	COLSIZE=$9002 	;36866
	ROWSIZE=$9003
	SCREENMAP=$9005

	ROWS=30
	COLUMNS=25
	VOLUME=36878
	SPEAKER1=36876
	SPEAKER2=36875
	SPEAKER3=36874
	SCREENMEM=$1800
	COLOURMEM=$9400
	SCROLLDELAY=5
	
	PRA  =  $dc00            ; CIA#1 (Port Register A)
	PRB  =  $dc01            ; CIA#1 (Port Register B)

	DDRA =  $dc02            ; CIA#1 (Data Direction Register A)
	DDRB =  $dc03            ; CIA#1 (Data Direction Register B)

	SCROLLMESSAGELENGTH=$B0-1
	O=SCREENMEM+((ROWS-1)*COLUMNS)-1 ; Offsets for grass
	P=COLOURMEM+((ROWS-1)*COLUMNS)-1

	jsr pointsong1 		; Start with the first song.

	lda 52 		; Save end of basic memory
	sta old52+1	;By directly modifying the code that will restore it.
	lda 56			
	sta old56+1
	lda SCREENMAP
	sta oldSCREENMAP+1 ; Save current pointer to character set and other VIC 

	
	lda #24			; Set end of basic to $1800
	; This gives us only 2K of space to work in!  Although very limited by
	; todays standards, it is JUST enough for this program.
	sta 52
	lda #24		      ; Set end of basic to $1800.  This is where our characters
	; will be stored
	sta 56		; Lower end of basic memory to make room
	; for our custom character set.

	
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

	lda #239		; 254 is 240 AND 14
	; The 14 refers to the lower three bits, which places the character map at 6144
	; 240 refers to bits 4-7, which with bit 7 of 36866 cleared, places the screen map
	; at 7168
	; This gives the screen map enough space for the extended screen size.
	; Refer to technical documentation for more info.

	sta SCREENMAP		; :Point VIC to our own character set
	lda #14			; Black background, blue border
	sta $900F 		; Set background/foreground


	ldx #0
colloop2:
	lda colors,x
	sta $9400,x
	lda colors+$100,x
	sta $9500,x
;	lda colors+$200,x
;	sta $9600,x
	dex
	bne colloop2  		; Loop over color map and set colours.
	; We will only worry about the first 768 bytes.
	
	lda COLSIZE
	sta oldCOLSIZE+1
	and #%01111111
	sta COLSIZE

	lda currentsong 	; If we have run this program before (by saving 2 here)
	cmp #2			; Skip the intro because it is now corrupted
	beq skipintro
	jsr waitkey
skipintro:
	sei
	lda #$20
	ldx #$8f

	stx $9125       ; Set up the timer
	sta $9126
	lda #<IrqHandler; And the IRQ handler
	sta $0314
	lda #>IrqHandler
	sta $0315
	cli	

	lda #COLUMNS		; This will clear the 7th bit, moving
	sta COLSIZE		; the location of the screen map to 7168 ($1c00)
	; It will also move the colour map to 37888 ($9400)


	lda ROWSIZE
	sta oldROWSIZE+1
	and #129
	ora #(ROWS*2)
	sta ROWSIZE

	lda HORIZOFFSET			; save screen information
	sta oldHORIZOFFSET+1
	clc
	sbc #3			; Shift screen to the left
	sta HORIZOFFSET
	
	lda VERTOFFSET
	sta oldVERTOFFSET+1
	clc
	sbc #12			; Shift screen up
	sta VERTOFFSET

	lda #(2*16)	; Set auxillary colour.  Multiply by 16 because it is bits 7-4
	; we need to set.

	sta VOLUME		; Auxillary colour is stored here, it occupies
	; the top 4 bits of volume.

	; Turn volume up!
	lda VOLUME
	ora #15
	sta VOLUME
	
	ldx #0
copychars:
	lda smiley,x
	sta $1C00,x
	inx
	cpx #(8*12)		; Copy first 10 characters
	bne copychars

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
	lda #1			; There are 5 grass characters.
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

	
colorloop:			; This is the main loop
	ldx xpos
	ldy ypos

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
	sta deltax		; Effectively alternating between the values
	; of 1 and 254 
	; If we add 254, it is effectively the same as subtracting 1
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
	dec sndcnt
	bne skipcheck
	lda #0
	STA SPEAKER3
skipcheck:
	rts

	;==================================================================
	;Short delay.  4/60th of a second to be exact;
	;==================================================================
wait:
	pha
	lda #0
	sta 162			; Store 0 in the timer register
waitl:
	lda 162			; Load it.  It will increment by one every jiffy			; (1/60th of a second)
	cmp #2			; Four jiffies passed?
	bne waitl
	pla
	rts

	;====================================================================
	; Start a sound
	; checksnd will turn this off eventually.  This just starts the sound
	;
	; Clobbers A
	;=====================================================================
beep:
	lda #204
	sta SPEAKER3
	lda #2			; This is how many frames before the sound fades out
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
	sta $9400-1,x
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
	; If we are drawing over the face characters
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
	beq hit			; Is zero?
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
	beq checkend		; Check if zero.
	inc blocker_x
	jmp checkend
left:
	lda blocker_x
	beq checkend
	dec blocker_x
	jmp checkend
down:
	lda blocker_y
	cmp #(ROWS-2)
	beq checkend
	inc blocker_y
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
;	jsr $ff84		; Restore default interrupt configuration
	jsr $ff8a

oldSCREENMAP:
	lda #0 ;This is a placeholder.
	; Restore character set and other VIC values to restore
	sta SCREENMAP		; screen size and position.
oldCOLSIZE:
	lda #0			;A placeholder, to be modified in place
	sta COLSIZE
oldROWSIZE:
	lda #0                  ;A placeholder, to be modified in place
	sta ROWSIZE
oldHORIZOFFSET:
	lda #0	; Restore screen offsets.  A placeholder to be overwritten.
	sta HORIZOFFSET
oldVERTOFFSET:
	lda #0 			; Another placeholder!
	sta VERTOFFSET

 	lda #$BF 	; Restore IRQ handler
	sta $0314	; Just assume it was the default.  We don't
	lda #$EA	; have enough memory for code to find out
	sta $0315	; where it original was.
	
	lda #$42
	sta $9125       ; Restore timer
	lda #$89
	sta $9126
	
old52:
	lda #0 			; 0 will be replaced by old52
	; The zero is a placeholder.  We will store the original value directly here,
	; by modifying the code.  We can save a byte of storage this way.
	sta 52
old56:
	lda #0			; 0 will be replaced by old56
	sta 56		; Restore end of basic memory

	; Restore border and screen colour
	lda #27
	sta $900F
	lda #6 			; Restore text color
	sta 646			; 

	lda #0			; Turn sounds off, in case a sound is still playing	

	sta SPEAKER2
	sta SPEAKER3
	sta SPEAKER1
	lda #147
	jsr $FFD2		; Clear screen, as there may be junk

	lda VOLUME
	and $F0
	sta VOLUME		; Set volume to zero
	lda #2
	sta currentsong		; We save 2 here, so that we can tell next time we run
	; if it has been run before.  That we we skip the now trashed intro screen.
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

IrqHandler:
	pha
	tya
	pha

	jsr writemsg

	dec notedur
	bne skipnote2

	ldy note
	lda (song_notes_treble),y
	sta SPEAKER1
	iny
	lda (song_notes_treble),y
	sta notedur
	iny
	cmp #0
	bne skipnote
	;	lda #1
	;	sta notedur
	tay
	lda #1
	cmp currentsong
	beq loadsong2
	jsr pointsong1
	dec currentsong
	jmp aa1
loadsong2:
	jsr pointsong2
	inc currentsong
aa1:
	sta notedur2
	sta notedur
	sta note
	sta note2
	jmp skipnote2x2
	
skipnote:
	tya
	sta note
skipnote2:
	dec notedur2
	bne skipnote2x2
	
	ldy note2
	lda (song_notes_bass),y
	sta SPEAKER2
	iny
	lda (song_notes_bass),y
	sta notedur2
	iny
	cmp #0
	bne skipnotex2
	;	lda #1
	;	sta notedur
	tay
	lda #1
	sta notedur2
	
skipnotex2:
	tya
	sta note2
skipnote2x2:
	pla             ; Restore registers
	tay
	pla
	jmp $EABF       ; Jump to the standard IRQ handling routine

pointsong1:
	; Only preserves Y because the places it is called,
	; are places we don't need to preserve  X
	tya
	pha
	ldy #>notes
	ldx #<notes
	stx song_notes_treble
	sty song_notes_treble+1
	ldy #>notes2
	ldx #<notes2
	stx song_notes_bass
	sty song_notes_bass+1
	pla
	tay
	rts

pointsong2:
	tya
	pha
	ldy #>notes_song2
	ldx #<notes_song2
	stx song_notes_treble
	sty song_notes_treble+1
	ldy #>notes2_song2
	ldx #<notes2_song2
	stx song_notes_bass
	sty song_notes_bass+1
	pla
	tay

	rts

	; Just waits for a key
waitkey:
	jsr $FF9F
	jsr $FFE4
	cmp #0
	beq waitkey
	rts

	
	;======================================================================
	; DATA BEGINS HERE
	;======================================================================

textoff: .byte 0
firepressed: .byte 0
firepressed_prev: .byte 0
starchar: .byte 9
currentsong: .byte 1

	; x and y coordinates of stars to place.  Maybe we do them randomly
	; in the future

stars:
.byte 20,20,13,22,14,4,15,14,15,16,15,23,15,5,15,6,16,22,16,3,17,20
.byte 22,11,23,24,23,3,26,24,26,6,4,3,6,19,7,14,17,14,8,18,9,3

	; This is the scrolling message in PETSCII.  It uses the
	; "REVERSE" character set.  Because we have shifted the
	; pointer to the character RAM down one character block,
	; selecing the reverse characters actually ends up
	; selecting the regular characters in ROM.
msg:
.byte 160,160,160,216,211,218,193,160,130,143,149
.byte 142,131,137,142,135,160,130,129,140,140,160
.byte 132,133,141,143,160,160,160,160,130,153,160
.byte 132,133,142,142,137,147,160,139,129,148,147
.byte 143,142,137,147,160,160,160,160,147,133,144
.byte 160,178,176,178,176,160,160,160,160,141,149
.byte 147,137,131,160,134,133,129,148,149,146,133
.byte 132,160,160,160,141,133,142,149,133,148,160
.byte 130,153,160,135,133,143,146,135,133,160,134
.byte 146,137,132,133,146,137,131,160,136,129,142
.byte 132,133,140,160,129,142,132,160,148,136,133
.byte 160,135,146,133,133,132,153,160,146,143,151
.byte 160,147,142,129,139,133,160,130,153,160,141
.byte 129,148,148,136,133,151,160,132,153,140,129
.byte 142,160,138,143,142,133,147,160,193,218,211
.byte 216,160,160,160,160,160,160,160,160,160,160

	; Notes are the Treble Clef notes for two songs.
	; Notes for the music. It is Menuet by George Frideric Handel.  
notes:
.byte g1,qua,a1,qua,b1,cro,c2,cro,d2,half,d2,cro,g2,cro,fs2,qua,e2,qua,d2,qua,c2,qua
.byte b1,half,c2,cro,d2,cro,e2,qua,d2,qua,c2,qua,b1,qua,a1,cro,b1,cro,c2,cro,b1,cro
.byte c2,qua,b1,qua,a1,qua,b1,qua,g1,half,g1,cro,a1,cro,a1,half,b1,cro,fs1,qua
.byte g1,qua,a1,cro,d2,half,d2,cro,e2,cro,b1,qua,c2,qua,d2,cro,g2,cro,fs2,cro
.byte e2,cro,ds2,cro,b1,cro,e2,cro,fs2,cro,g2,cro,a2,qua,b2,qua,g2,cro,fs2,qua
.byte g2,qua,e2,cro,e1,cro,fs1,cro,g1,cro,a1,cro,b1,cro,c2,cro,fs1,cro,g1,cro
.byte a1,cro,b1,cro,c2,cro,d2,cro,b1,cro,c2,cro,d2,cro,e2,cro,fs2,cro,g2,cro
.byte b1,cro,c2,qua,b1,qua,a1,qua,b1,qua,g1,half,g1,cro,0,whole,0,whole,0,0

notes_song2:
	; Notes for the music. It is The Greedy Row Snake from Childs Play, by Matthew
	; Dylan Jones
.byte 0,half,0,cro,0,half,0,cro,0,half,0,cro,0,half,0,cro
.byte d2,half,d2,cro,b1,half,b1,cro
.byte cs2,half,as1,cro,gs1,half,c2,cro
.byte a1,half,g1,cro,ds2,half,f1,cro,e1,cro,fs1,half,0,half,0,cro
.byte e1,qua,0,qua,fs1,half,f1,qua,0,qua,g1,half,ds2,cro,cs2,half,as1,cro,as1,half
.byte d2,half,d2,cro,c2,half,c2,cro,a1,half,a1,cro,b1,half,b1,cro
.byte b1,half,b1,cro,0,whole,0,whole,0,0

	; Bass clef notes for the above songs.
notes2:
.byte g1,cro,g0,cro,a0,cro,b0,cro,c1,cro,d1,cro,e1,cro,g1,cro,fs1,cro,g1,cro
.byte g0,cro,a0,cro,b0,cro,g0,cro,c1,cro,d1,half,e1,cro,d1,cro,c1,cro,d1,cro
.byte g0,cro,b0,cro,g0,cro,d1,cro,fs1,cro,a1,cro,d1,cro,a0,cro,d0,cro,d1,cro
.byte e1,cro,fs1,cro,g1,cro,d1,cro,g0,cro,e1,cro,b1,cro,c2,cro,b1,cro,a1,cro
.byte g1,cro,a1,cro,fs1,cro,b1,cro,e1,cro,b0,cro,c0,cro,e1,half,d1,cro,c1
.byte cro,b0,cro,a0,cro,d1,cro,e1,cro,fs1,cro,g1,cro,a1,cro,b1,cro,g1,cro
.byte a1,cro,b1,cro,c2,cro,a1,cro,g1,cro,d1,cro,c1,cro,d1,cro,g0,cro,b0,cro
.byte g0,cro,0,whole,0,whole,0,0

notes2_song2:
.byte b1,cro,gs1,cro,as1,cro,g1,cro,f1,cro,a1,cro
.byte fs1,cro,e1,cro,c1,cro,d1,cro,cs1,cro,ds1,cro
.byte b1,cro,gs1,cro,as1,cro,g1,cro,f1,cro,a1,cro
.byte fs1,cro,e1,cro,c1,cro,d1,cro,cs1,cro,ds1,cro
.byte b1,cro,gs1,cro,as1,cro,g1,cro,f1,cro,a1,cro
.byte fs1,cro,e1,cro,c1,cro,d1,cro,cs1,cro,ds1,cro
.byte b1,cro,gs1,cro,as1,cro,g1,cro,f1,cro,a1,cro
.byte fs1,cro,e1,cro,c1,cro,d1,cro,cs1,cro,ds1,cro
.byte b1,cro,gs1,cro,as1,cro,g1,cro,f1,cro,a1,cro
.byte fs1,cro,e1,cro,c1,cro,d1,cro,cs1,cro,ds1,cro,b0,half,b0,cro,0,whole,0,whole,0,0
	

note: .byte 0
notedur: .byte 1
note2: .byte 0
notedur2: .byte 1

smiley: .INCBIN "characters-charset.bin",0,(12*8)

.SEGMENT "SCREENRAM"

introscree:
.byte 102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128
.byte 102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128,102+128
.byte 102+128,88+128,88+128,88+128,32+128,19+128,13+128,9+128,12+128,5+128,2+128,15+128,21+128,14+128,3+128,5+128,32+128,88+128,88+128,88+128,32+128,102+128
.byte 102+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,102+128
.byte 102+128,1+128,32+128,19+128,13+128,1+128,12+128,12+128,32+128,4+128,5+128,13+128,15+128,32+128,6+128,15+128,18+128,32+128,20+128,8+128,5+128,102+128
.byte 102+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,22+128,9+128,3+128,32+128,50+128,48+128,32+128,32+128,32+128,32+128,32+128,32+128,102+128
.byte 102+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,46+128,32+128,102+128
.byte 102+128,32+128,23+128,32+128,61+128,32+128,21+128,16+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,102+128
.byte 102+128,32+128,19+128,32+128,61+128,32+128,4+128,15+128,23+128,14+128,32+128,32+128,32+128,32+128,46+128,32+128,32+128,32+128,32+128,32+128,32+128,102+128
.byte 102+128,32+128,1+128,32+128,61+128,32+128,12+128,5+128,6+128,20+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,81+128,32+128,102+128
.byte 102+128,32+128,4+128,32+128,61+128,32+128,18+128,9+128,7+128,8+128,20+128,32+128,32+128,102+128,85+128,64+128,73+128,32+128,32+128,32+128,32+128,102+128
.byte 102+128,32+128,19+128,20+128,15+128,16+128,32+128,61+128,32+128,17+128,21+128,9+128,20+128,107+128,91+128,91+128,91+128,115+128,32+128,32+128,32+128,102+128
.byte 102+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,74+128,64+128,75+128,32+128,32+128,32+128,32+128,102+128
.byte 102+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,85+128,64+128,64+128,73+128,32+128,32+128,32+128,32+128,46+128,32+128,102+128
.byte 102+128,32+128,32+128,32+128,78+128,77+128,32+128,32+128,32+128,85+128,64+128,75+128,58+128,58+128,74+128,64+128,73+128,32+128,32+128,32+128,32+128,102+128
.byte 102+128,32+128,32+128,32+128,95+128,105+128,32+128,32+128,32+128,93+128,85+128,67+128,67+128,67+128,67+128,73+128,93+128,32+128,32+128,32+128,32+128,102+128
.byte 102+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,93+128,93+128,32+128,123+128,108+128,32+128,93+128,93+128,32+128,32+128,32+128,32+128,102+128
.byte 102+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,32+128,93+128,74+128,73+128,126+128,124+128,85+128,75+128,93+128,32+128,32+128,32+128,32+128,102+128
.byte 102+128,32+128,32+128,46+128,32+128,32+128,32+128,32+128,32+128,74+128,73+128,93+128,95+128,105+128,93+128,85+128,75+128,32+128,78+128,77+128,32+128,102+128
.byte 102+128,32+128,32+128,32+128,32+128,78+128,77+128,32+128,32+128,32+128,93+128,93+128,118+128,117+128,93+128,93+128,32+128,78+128,78+128,77+128,77+128,102+128
.byte 102+128,32+128,32+128,32+128,78+128,78+128,77+128,77+128,32+128,32+128,32+128,32+128,118+128,117+128,32+128,32+128,78+128,78+128,78+128,77+128,77+128,102+128
.byte 102+128,32+128,32+128,78+128,78+128,78+128,77+128,77+128,77+128,78+128,77+128,32+128,118+128,117+128,32+128,78+128,78+128,78+128,78+128,77+128,77+128,102+128
.byte 102+128,88+128,78+128,78+128,78+128,78+128,77+128,77+128,77+128,32+128,32+128,77+128,118+128,117+128,78+128,78+128,78+128,78+128,78+128,77+128,77+128,102+128

colors:
	; Color data
.byte 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
.byte 6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6
.byte 6,5,5,5,3,3,3,3,3,3,3,3,3,3,3,3,6,5,5,5,6,6
.byte 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
.byte 6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,6
.byte 6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,6,6,6,6,6,6,6
.byte 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,1,6,6
.byte 6,6,7,2,5,2,2,2,6,6,6,6,6,6,6,6,6,6,6,6,6,6
.byte 6,6,7,2,5,2,2,2,2,2,6,6,6,6,1,6,6,6,6,6,6,6
.byte 6,6,7,2,5,2,2,2,2,2,6,6,6,6,6,6,6,6,6,1,6,6
.byte 6,6,7,2,5,2,2,2,2,2,2,6,6,0,6,6,6,6,6,6,6,6
.byte 6,6,7,7,7,7,2,5,5,2,2,2,2,7,6,6,6,7,6,6,6,6
.byte 6,6,6,6,2,2,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
.byte 6,6,6,6,2,2,6,6,6,6,6,5,5,5,5,6,6,6,6,2,6,6
.byte 6,6,6,6,6,6,6,6,6,5,5,5,2,2,5,5,5,6,6,6,6,6
.byte 6,6,6,6,6,6,6,6,6,5,3,3,3,3,3,3,5,6,6,6,6,6
.byte 6,6,6,6,6,6,6,6,6,5,3,6,5,5,6,3,5,6,6,6,6,6
.byte 6,6,6,6,6,6,6,6,6,5,3,3,5,5,3,3,5,6,6,6,6,6
.byte 6,6,6,1,6,4,6,4,4,5,5,3,7,7,3,5,5,6,4,4,6,6
.byte 6,6,6,6,6,4,4,4,4,4,5,3,7,7,3,5,4,4,6,6,4,6
.byte 6,6,6,6,4,6,6,4,4,4,4,6,7,7,6,4,4,6,3,3,6,6
.byte 6,6,6,4,6,3,3,6,4,4,4,4,7,7,4,4,6,3,1,1,3,6
.byte 6,5,4,6,3,1,1,3,6,4,4,4,7,7,4,6,3,1,7,7,1,6



	; Note data
	c0=128
	cs0=134
	d0=141
	ds0=147
	e0=153
	f0=159
	fs0=164
	g0=170
	gs0=174
	a0=179
	as0=183
	b0=187
	c1=191
	cs1=195
	d1=198
	ds1=201
	e1=204
	f1=207
	fs1=210
	g1=213
	gs1=215
	a1=217
	as1=219
	b1=221
	c2=223
	cs2=225
	d2=227
	ds2=229
	e2=230
	f2=231
	fs2=232
	g2=234
	gs2=235
	a2=236
	as2=237
	b2=238
	c3=239
	cs3=240
	
	qua = 5
	cro = qua*2
	half = cro*2
	whole = half*2

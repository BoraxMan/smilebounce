	;-----------------------------------------------------
	;
	; Bounce a smiley face around the screen
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
	; $1800 - $1BFF : Custom Characters (We only use the first 512 bytes)
	; $1C00 - $1FFF : Screen map (We only need 750 bytes)

	bsout=$FFD2
	offset=$FB
	xpos = $FD
	ypos = $FE
	deltax = $F9 		; RS-232 tx pointer ZP location.  We aren't using this.
	deltay = $F8		; We store delta x and y here.
	sndcnt = $FA

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
	SCREENMEM=$1C00
	COLOURMEM=$9400

	O=SCREENMEM+((ROWS-1)*COLUMNS)-1 ; Offsets for grass
	P=COLOURMEM+((ROWS-1)*COLUMNS)-1
	
	lda #1
	sta xpos
	sta ypos		; Set starting coordinates to 1,1
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
	sta COLSIZE		; the location of the screen map to 7160 ($1c00)
	; It will also move the colour map to 37888 ($9400)
	lda ROWSIZE
	sta oldROWSIZE
	and #129
	ora #(ROWS*2)
	sta ROWSIZE

	lda HORIZOFFSET			; information
	sta oldHORIZOFFSET
	clc
	sbc #3			; Shift to the left
	sta HORIZOFFSET
	
	lda VERTOFFSET
	sta oldVERTOFFSET
	clc
	sbc #12			; Shift up
	sta VERTOFFSET

	lda #(2*16)	; Set auxillary colour.  Multiply by 16 because it is bits 7-4
	; we need to set.
	; The documentation I've seen indicates that ths should be light cyan
	; but it comes out pink on vice.
	sta VOLUME		; Auxillary colour is stored here, it occupies
	; the top 4 bits of volume.
	
	ldx #0
copychar:
	lda $8000,x
	sta $1800,x
	lda $8100,x
	sta $1900,x
	dex
	bne copychar		; Copy Character memory to location $1800

	ldx #0
copychars:
	lda smiley,x
	sta $1800,x
	inx
	cpx #(8*9)		; Copy first nine characters
	bne copychars

	
	lda #254		; 254 is 240 AND 14
	; The 14 refers to the lower three bits, which places the character map at 6144
	; 250 refers to bits 4-7, which with bit 7 of 36866 cleared, places the screen map
	; at 7168
	; This gives the screen map enough space for the extended screen size.
	; Refer to technical documentation for more info.
	sta SCREENMAP		; :Point VIC to our own character set
 	lda #14			; Black background, blue border
	sta $900F 		; Set background/foreground

	ldx #0
	lda #12
colloop1:
	sta $9400,x
	sta $9500,x
	sta $9600,x
	dex
	bne colloop1  		; Loop over color map and set to purple.
	; We will only worry about the first 768 bytes.
	
	ldx #0
	lda #32
loop1:  sta $1C00,x
	sta $1D00,x
	sta $1E00,x
	dex
	bne loop1 		; Loop over character map and set to space
	; We will only worry about the first 768 bytes.
	
	ldx #25
drawgrass:
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

	cpx #(COLUMNS-2)			;Last Column
	beq flipx
	cpx #0
	beq flipx
xdone:
	cpy #(ROWS-3)			;Second last row (grass is on the last row
	beq flipy
	cpy #0
	beq flipy
ydone:
	clc
	txa
	adc deltax		; Add the 'delta x', which would either
	; increment or decrement this by 1.
	tax
	
	clc
	tya
	adc deltay
	tay

	stx xpos
	sty ypos

	jsr plotit
	jsr drawface
	jsr wait
	jsr eraseface

	jsr checksnd
	jsr $ffe1		
	bne colorloop
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
	ldx #<msg
	ldy #>msg
	jsr print
	rts
	
	;==============================================================
	; This will negate DELTAX to switch the X direction of the face.
	;===============================================================
flipx:
	lda deltax
	eor #$fe		; Negate all the bits except LSB
	sta deltax		;   Effectively alternating between the values
	; of 1 and 254 
	;  If we add 254, it is effectively the same as subtracting 1
	jsr beep
	jmp xdone

	;==============================================================
	; This will negate DELTAY to switch the X direction of the face.
	;===============================================================
flipy:
	lda deltay
	eor #$fe
	sta deltay
	jsr beep
	jmp ydone

	;=================================================================
	; This calculates the screen memory offset of the LINE that we will
	; draw on.
	;==================================================================
plotit:
	lda #00
	sta offset
	lda #$1C
	sta offset+1
	clc
	ldx ypos
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
	;Routine to draw the face
	;======================================================================
drawface:
	clc
	lda #0			; Load the PETSCII for a heart.
	ldy xpos
	sta (offset),y		; Save the heart at offset + y (which actually holds
	; the X position).
	tya
	adc #1
	tay
	lda #3
	sta (offset),y
	tya
	adc #24
	tay
	lda #4
	sta (offset),y
	tya
	adc #1
	tay
	lda #5
	sta (offset),y
	rts

	;======================================================================
	;Routine to erase the face
	;======================================================================
eraseface:
	clc
	lda #32			; Load the PETSCII for a heart.
	ldy xpos
	sta (offset),y		; Save the heart at offset + y (which actually holds
	; the X position).
	tya
	adc #1
	tay
	lda #32
	sta (offset),y
	tya
	adc #24
	tay
	lda #32
	sta (offset),y
	tya
	adc #1
	tay
	lda #32
	sta (offset),y
	rts
	
	;==================================================================
	; Print text
	; Based on Asembler Example from C64 Wiki
	;==================================================================
print:
	stx offset               ;save string pointer LSB
        sty offset+1             ;save string pointer MSB
        ldy #0                ;starting string index
printloop:
	lda (offset),y           ;get a character
        beq printend          ;end of string
        jsr bsout             ;print character
        iny                   ;next
        bne printloop
printend:
	rts                   ;exit

	
old52: .byte 0
old56: .byte 0
oldSCREENMAP: .byte 0
oldHORIZOFFSET: .byte 0
oldVERTOFFSET: .byte 0
oldCOLSIZE: .byte 0
oldROWSIZE: .byte 0
msg: .byte "bouncing face demo",$0D,"by dennis katsonis",$0D,$0D,"sep 2020",0
smiley: .INCBIN "characters.bin"




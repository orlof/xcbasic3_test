rem **************************************
rem * Color constants
rem **************************************
SHARED CONST COLOR_BLACK       = 0
SHARED CONST COLOR_WHITE       = 1
SHARED CONST COLOR_RED         = 2
SHARED CONST COLOR_CYAN        = 3
SHARED CONST COLOR_PURPLE      = 4
SHARED CONST COLOR_GREEN       = 5
SHARED CONST COLOR_BLUE        = 6
SHARED CONST COLOR_YELLOW      = 7
SHARED CONST COLOR_ORANGE      = 8 
SHARED CONST COLOR_BROWN       = 9
SHARED CONST COLOR_LIGHTRED    = 10
SHARED CONST COLOR_DARKGRAY    = 11
SHARED CONST COLOR_MIDDLEGRAY  = 12
SHARED CONST COLOR_LIGHTGREEN  = 13
SHARED CONST COLOR_LIGHTBLUE   = 14
SHARED CONST COLOR_LIGHTGRAY   = 15

const SPR_ENABLE        = $d015 
const SPR_X_COORD       = $d000
const SPR_Y_COORD       = $d001
const SPR_X_COORD_MSB   = $d010
const SPR_MULTICOLOR    = $d01c
const SPR_MCOLOR1       = $d025
const SPR_MCOLOR2       = $d026
const SPR_EXP_X         = $d01d
const SPR_EXP_Y         = $d017
const SPR_DATA_PRIO     = $d01b
const SPR_SPR_COLL      = $d01e
const SPR_DATA_COLL     = $d01f
const SPR_COLOR         = $d027

CONST TEXT_BANK        = 0
CONST TEXT_SCRMEM      = 1

DIM ZpAddr AS WORD FAST
DIM HiresUnsetAddr AS WORD FAST
    HiresUnsetAddr = 0

DIM BankAddr AS WORD
    BankAddr = 16384 * TEXT_BANK
DIM BitmapAddr AS WORD
DIM ScrMemAddr AS WORD
    ScrMemAddr = BankAddr + 1024 * TEXT_SCRMEM
DIM SprPtrAddr AS WORD
    SprPtrAddr = ScrMemAddr + 1016

DIM Y_Table_Hi(200) AS BYTE
DIM Y_Table_Lo(200) AS BYTE

DIM X_Table(200) AS BYTE
DIM PixelMask(200) AS BYTE
    FOR t AS BYTE = 0 TO 192 STEP 8
        FOR t2 AS BYTE = 0 TO 7
            X_Table(t+t2) = t
            PixelMask(t+t2) = SHR($80, t2)
        NEXT t2
    NEXT t

SUB hires_on(Bank AS BYTE, Bitmap AS BYTE, ScrMem AS BYTE) SHARED STATIC
    rem -- BANK 0 to 3
    poke $dd00, (peek($dd00) AND %11111100) OR (Bank XOR %11)

    rem -- BITMAP 0 to 1, SCRMEM 0 to 15
    poke $d018, SHL(ScrMem, 4) OR SHL(Bitmap, 3)

    rem -- Bitmap mode on
    poke $d011, peek($d011) OR %00100000

    rem -- Multicolor mode off
    poke $d016, peek($d016) AND %11101111

    BankAddr = 16384 * Bank
    BitmapAddr = BankAddr + 8192 * Bitmap
    ScrMemAddr = BankAddr + 1024 * ScrMem
    SprPtrAddr = ScrMemAddr + 1016

    FOR t AS BYTE = 0 TO 199
        DIM addr AS WORD
        addr = BitmapAddr + t MOD 8 + CWORD(320) * (t / 8)
        Y_Table_Lo(t) = PEEK(@addr)
        Y_Table_Hi(t) = PEEK(@addr+1)
    NEXT t
END SUB

SUB hires_off() SHARED STATIC
    rem -- Bitmap mode off
    poke $d011, peek($d011) AND %11011111
    rem -- Restore screen address to default
    poke $d018, SHL(TEXT_SCRMEM, 4) OR %0101
    rem -- Switch VIC to bank 0
    poke $dd00, (peek($dd00) AND %11111100) OR (TEXT_BANK XOR %11)

    BankAddr = 16384 * TEXT_BANK
    ScrMemAddr = BankAddr + 1024 * TEXT_SCRMEM
    SprPtrAddr = ScrMemAddr + 1016
END SUB

SUB hires_clear() SHARED STATIC
    MEMSET BitmapAddr, 8000, 0
END SUB

SUB hires_color(inkcol AS BYTE, bgcol AS BYTE) SHARED STATIC
    MEMSET ScrMemAddr, 1000, SHL(inkcol, 4) OR bgcol
END SUB

SUB hires_unset() SHARED STATIC
    ASM
        lda {HiresUnsetAddr}
        beq hires_unset_end
        
        lda #0
        jmp ({HiresUnsetAddr})
        sta $dead
        sta $dead
        sta $dead
        sta $dead
        sta $dead
        sta $dead
        sta $dead
        sta $dead
        sta $dead
        sta $dead
hires_unset_end
        lda #<hires_unset_end
        sta #<{HiresUnsetAddr}
        lda #>hires_unset_end
        sta #>{HiresUnsetAddr}
    END ASM
END SUB

SUB hires_set(x AS BYTE, y AS BYTE) SHARED STATIC
    ASM
        ldx {x}
        cpx #200
        bcs hires_set_end

        ldy {y}
        cpy #200
        bcs hires_set_end

        lda {Y_Table_Lo},y
        clc
        adc {X_Table},x
        sta $fb
        lda #0
        adc {Y_Table_Hi},y
        sta $fc

        lda {PixelMask},x
        ldy #0

        ora ($fb),y
        sta ($fb),y

        lda $fd                     ; add to erase queue
        sec
        sbc #3
        sta $fd
        bcs hires_set_no_borrow
        dec $fe
hires_set_no_borrow
        ldy #1
        lda $fb
        sta ($fd),y
        iny
        lda $fc
        sta ($fd),y
hires_set_end
    END ASM
END SUB

DIM PETSCII_TO_SCREENCODE(8) AS BYTE @ _PETSCII_TO_SCREENCODE
_PETSCII_TO_SCREENCODE:
DATA AS BYTE $80, $00, $c0, $e0, $40, $c0, $80, $80

SUB hires_text(x AS BYTE, y AS BYTE, len AS BYTE, text AS STRING * 40) SHARED STATIC
    rem disable interrupt and enable char rom
    ASM
        sei
    END ASM
    POKE 1, PEEK(1) AND %11111011

    DIM dst AS WORD: dst = SHL(Y_Table_Hi(8*y), 8) + Y_Table_Lo(8*y) + CWORD(8) * CWORD(x)
    MEMSET dst, CWORD(8) * len, 0
    FOR t AS BYTE = 1 TO LEN(text)
        DIM c AS BYTE: c = PEEK(@text + t)
        IF c=$ff THEN
            c = $5e
        ELSE
            c = c + PETSCII_TO_SCREENCODE(SHR(c, 5)) 
        END IF
        MEMSHIFT $D800 + CWORD(8) * CWORD(c), dst, 8
        dst = dst + 8
    NEXT t

    rem disable char ROM and enable interrupts
    POKE 1, PEEK(1) OR %00000100
    ASM
        cli
    END ASM
END SUB

SUB sleep(amount AS LONG) SHARED STATIC
    DIM endtime AS LONG
    endtime = ti() + amount
    DO WHILE ti() < endtime
        rem empty loop
    LOOP
END SUB



REM Main program must write a nonzero value 
REM here when it wants new sprites to be displayed
DIM sprupdateflag AS BYTE FAST
DIM sortedsprites AS BYTE FAST
DIM tempvariable AS BYTE FAST
DIM sprirqcounter AS BYTE FAST

DIM sortorder(16) AS BYTE FAST

REM Unsorted sprite table
DIM sprx(16) AS BYTE
DIM spry(16) AS BYTE
DIM sprc(16) AS BYTE
DIM sprf(16) AS BYTE

DIM RotX(256) AS BYTE @ _RotX
DIM RotY(256) AS BYTE @ _RotY

SUB SpriteColor(spr_nr AS BYTE, color AS BYTE) SHARED STATIC
    sprc(spr_nr) = color
END SUB

SUB SpriteShape(spr_nr AS BYTE, shape AS BYTE) SHARED STATIC
    sprf(spr_nr) = shape
END SUB

SUB SpriteAt(spr_nr AS BYTE, x AS BYTE, y AS BYTE) SHARED STATIC
    sprx(spr_nr) = x
    spry(spr_nr) = y
END SUB

SUB SpriteMove(spr_nr AS BYTE, dx AS BYTE, dy AS BYTE) SHARED STATIC
    sprx(spr_nr) = sprx(spr_nr) + dx
    spry(spr_nr) = spry(spr_nr) + dy
END SUB

SUB SpriteMoveForward(spr_nr AS BYTE, angle AS BYTE, speed AS BYTE) SHARED STATIC
    DIM index AS BYTE: index = (angle AND %11111000) OR (speed AND %00000111)
    sprx(spr_nr) = sprx(spr_nr) + (RotX(index) - 11)
    spry(spr_nr) = spry(spr_nr) + (RotY(index) - 10)
END SUB

SUB ShapeClear(Shape AS BYTE) SHARED STATIC
    MEMSET BankAddr + 64 * CWORD(Shape), 63, 0
END SUB

DIM sprite_line_x1 AS BYTE FAST
DIM sprite_line_y1 AS BYTE FAST
DIM sprite_line_x2 AS BYTE FAST
DIM sprite_line_y2 AS BYTE FAST
DIM sprite_line_dx AS BYTE FAST
DIM sprite_line_dy AS BYTE FAST
DIM sprite_line_err AS BYTE FAST

SUB ShapeDrawLine(Shape AS BYTE) SHARED STATIC
    ZpAddr = BankAddr + 64 * CWORD(Shape)
    ASM
        ldx #$c6                ; calc dy, sy
        lda {sprite_line_y1}
        sec
        sbc {sprite_line_y2}
        bpl sprite_line_dy_negative
        ldx #$e6
        eor #$ff                ; neg
        clc
        adc #1
sprite_line_dy_negative
        sta {sprite_line_dy}
        stx sprite_line_commit_sy

        ldx #$c6                ; calc dx, sx
        lda {sprite_line_x1}
        sec
        sbc {sprite_line_x2}
        bpl sprite_line_dx_negative
        ldx #$e6
        eor #$ff                ; neg
        clc
        adc #1
sprite_line_dx_negative
        sta {sprite_line_dx}
        stx sprite_line_commit_sx

        cmp {sprite_line_dy}
        beq sprite_line_err_dy
        bpl sprite_line_err_dx
sprite_line_err_dy
        lda {sprite_line_dy}
        eor #$ff                ; neg
        clc
        adc #1
sprite_line_err_dx
        sta {sprite_line_err}

        asl {sprite_line_dx}
        asl {sprite_line_dy}

sprite_line_loop
        ; plot
        lda {sprite_line_x1}	    ; addr offset to y
        lsr
        lsr
        lsr
        clc
        adc {sprite_line_y1}
        adc {sprite_line_y1}
        adc {sprite_line_y1}
        tay
        
	    lda {sprite_line_x1}      ; bitmask offset to x
        and #%00000111
        tax
        
        lda ({ZpAddr}),y
        ora {PixelMask},x
        sta ({ZpAddr}),y
        
        ; x1 != x2 ?
        lda {sprite_line_x1}
        cmp {sprite_line_x2}
        bne sprite_line_step

        ; y1 != y2 ?
        lda {sprite_line_y1}
        cmp {sprite_line_y2}
        bne sprite_line_step

        rts

sprite_line_step
        lda {sprite_line_err}
        pha

        clc
        adc {sprite_line_dx}
        bmi sprite_line_no_dx
        beq sprite_line_no_dx

        lda {sprite_line_err}
        sec
        sbc {sprite_line_dy}
        sta {sprite_line_err}
        
sprite_line_commit_sx
        inc {sprite_line_x1}

sprite_line_no_dx
        pla
        cmp {sprite_line_dy}
        bpl sprite_line_no_dy
        
        lda {sprite_line_err}
        clc
        adc {sprite_line_dx}
        sta {sprite_line_err}

sprite_line_commit_sy
        inc {sprite_line_y1}

sprite_line_no_dy
        jmp sprite_line_loop
    END ASM
END SUB

CONST SHAPE_NEXT = %01000000
CONST SHAPE_SKIP = %00100000

SUB ShapeDrawGeometry(Shape AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
    DIM pAngle AS BYTE
    DIM pRadius AS BYTE
    DIM pIndex AS BYTE

    pAngle = PEEK(GeometryAddr) + Angle
    pRadius = PEEK(GeometryAddr + 1)
    pIndex = (pAngle AND %11111000) OR (pRadius AND %00000111)
    sprite_line_x2 = RotX(pIndex)
    sprite_line_y2 = RotY(pIndex)
    GeometryAddr = GeometryAddr + 2

    DO
        sprite_line_x1 = sprite_line_x2
        sprite_line_y1 = sprite_line_y2

        pAngle = PEEK(GeometryAddr) + Angle
        pRadius = PEEK(GeometryAddr + 1)
        pIndex = (pAngle AND %11111000) OR (pRadius AND %00000111)
        sprite_line_x2 = RotX(pIndex)
        sprite_line_y2 = RotY(pIndex)

        IF (pRadius AND SHAPE_SKIP) = 0 THEN
            CALL ShapeDrawLine(Shape)
        END IF
        GeometryAddr = GeometryAddr + 2
    LOOP WHILE (PEEK(GeometryAddr + 1) AND SHAPE_NEXT) = 0
END SUB

SUB SpriteUpdate() SHARED STATIC
    ASM
                inc {sprupdateflag}     ;Signal to IRQ: sort the
                                        ;sprites
waitloop:       lda {sprupdateflag}     ;Wait until the flag turns back
                bne waitloop            ;to zero
    END ASM
END SUB

SUB SpriteInit() SHARED STATIC
    sortedsprites = 0
    sprupdateflag = 0
    FOR t AS BYTE = 0 TO 15
        sortorder(t) = t
    NEXT t

    ASM
        ;Main program
        ;Routine to init the raster interrupt system
initraster:
                lda {SprPtrAddr}
                sta irq2_sprf+1
                lda {SprPtrAddr}+1
                sta irq2_sprf+2

                sei
                lda #<irq1
                sta $0314
                lda #>irq1
                sta $0315
                lda #$7f                    ;CIA interrupt off
                sta $dc0d
                lda #$01                    ;Raster interrupt on
                sta $d01a
                lda $d011
                and #%01111111              ;High bit of interrupt position = 0
                sta $d011
                lda #IRQ1LINE               ;Line where next IRQ happens
                sta $d012
                lda $dc0d                   ;Acknowledge IRQ (to be sure)
                cli

                rts

IRQ1LINE        = $fc           ;This is the place on screen where the sorting
                                ;IRQ happens
MAXSPR          = 16            ;Maximum number of sprites


        ;Raster interrupt 1. This is where sorting happens.

irq1:           dec $d019                       ;Acknowledge raster interrupt
                lda #$ff                        ;Move all sprites
                sta $d001                       ;to the bottom to prevent
                sta $d003                       ;weird effects when sprite
                sta $d005                       ;moves lower than what it
                sta $d007                       ;previously was
                sta $d009
                sta $d00b
                sta $d00d
                sta $d00f

                lda {sprupdateflag}             ;New sprites to be sorted?
                beq irq1_nonewsprites
                lda #$00
                sta {sprupdateflag}
                lda #16                         ;Take number of sprites given
                                                ;by the main program
                sta {sortedsprites}             ;If it�s zero, don�t need to
                bne irq1_beginsort              ;sort

irq1_nonewsprites:
                ldx {sortedsprites}
                cpx #$09
                bcc irq1_notmorethan8
                ldx #$08
irq1_notmorethan8:
                lda d015tbl,x                   ;Now put the right value to
                sta $d015                       ;$d015, based on number of
                beq irq1_nospritesatall         ;sprites
                                                ;Now init the sprite-counter
                lda #$00                        ;for the actual sprite display
                sta {sprirqcounter}               ;routine
                lda #<irq2                      ;Set up the sprite display IRQ
                sta $0314
                lda #>irq2
                sta $0315
                jmp irq2_direct                 ;Go directly; we might be late
irq1_nospritesatall:
                jmp $ea81

irq1_beginsort: ; inc $d020
                ldx #MAXSPR
                dex
                cpx {sortedsprites}
                bcc irq1_cleardone
                lda #$ff                        ;Mark unused sprites with the
irq1_clearloop: sta {spry},x                      ;lowest Y-coordinate ($ff);
                dex                             ;these will "fall" to the
                cpx {sortedsprites}               ;bottom of the sorted table
                bcs irq1_clearloop
irq1_cleardone: ldx #$00
irq1_sortloop:  ldy {sortorder}+1,x               ;Sorting code. Algorithm
                lda {spry},y                      ;ripped from Dragon Breed :-)
                ldy {sortorder},x
                cmp {spry},y
                bcs irq1_sortskip
                stx irq1_sortreload+1
irq1_sortswap:  lda {sortorder}+1,x
                sta {sortorder},x
                sty {sortorder}+1,x
                cpx #$00
                beq irq1_sortreload
                dex
                ldy {sortorder}+1,x
                lda {spry},y
                ldy {sortorder},x
                cmp {spry},y
                bcc irq1_sortswap
irq1_sortreload:ldx #$00
irq1_sortskip:  inx
                cpx #MAXSPR-1
                bcc irq1_sortloop
                ldx {sortedsprites}
                lda #$ff                       ;$ff is the endmark for the
                sta sortspry,x                 ;sprite interrupt routine
                ldx #$00
irq1_sortloop3: ldy {sortorder},x                ;Final loop:
                lda {spry},y                     ;Now copy sprite variables to
                sta sortspry,x                 ;the sorted table
                lda {sprx},y
                sta sortsprx,x
                lda {sprf},y
                sta sortsprf,x
                lda {sprc},y
                sta sortsprc,x

                inx
                cpx {sortedsprites}
                bcc irq1_sortloop3
                ; dec $d020
                jmp irq1_nonewsprites

        ;Raster interrupt 2. This is where sprite displaying happens

irq2:           dec $d019                       ;Acknowledge raster interrupt
irq2_direct:    ldy {sprirqcounter}               ;Take next sorted sprite number
                lda sortspry,y                  ;Take Y-coord of first new sprite
                clc
                adc #$10                        ;16 lines down from there is
                bcc irq2_notover                ;the endpoint for this IRQ
                lda #$ff                        ;Endpoint can�t be more than $ff
irq2_notover:   sta {tempvariable}
irq2_spriteloop:lda sortspry,y
                cmp {tempvariable}                ;End of this IRQ?
                bcs irq2_endspr
                ldx physicalsprtbl2,y           ;Physical sprite number x 2
                sta $d001,x                     ;for X & Y coordinate
                lda sortsprx,y
                ;asl
                sta $d000,x
                bcc irq2_lowmsb
                lda $d010
                ora ortbl,x
                sta $d010
                jmp irq2_msbok
irq2_lowmsb:    lda $d010
                and andtbl,x
                sta $d010
irq2_msbok:     ldx physicalsprtbl1,y           ;Physical sprite number x 1
                lda sortsprf,y
irq2_sprf:
                sta $07f8,x                     ;for color & frame
                lda sortsprc,y
                sta $d027,x
                iny
                bne irq2_spriteloop
irq2_endspr:    cmp #$ff                        ;Was it the endmark?
                beq irq2_lastspr
                sty {sprirqcounter}
                sec                             ;That coordinate - $10 is the
                sbc #$10                        ;position for next interrupt
                cmp $d012                       ;Already late from that?
                bcc irq2_direct                 ;Then go directly to next IRQ
                sta $d012
                jmp $ea81
irq2_lastspr:   lda #<irq1                      ;Was the last sprite,
                sta $0314                       ;go back to irq1
                lda #>irq1                      ;(sorting interrupt)
                sta $0315
                lda #IRQ1LINE
                sta $d012
                jmp $ea81

sortsprx:       ds.b MAXSPR,0                   ;Sorted sprite table
sortspry:       ds.b MAXSPR+1,0                 ;Must be one byte extra for the
                                                ;$ff endmark
sortsprc:       ds.b MAXSPR,0
sortsprf:       ds.b MAXSPR,0


d015tbl:        dc.b %00000000                  ;Table of sprites that are "on"
                dc.b %00000001                  ;for $d015
                dc.b %00000011
                dc.b %00000111
                dc.b %00001111
                dc.b %00011111
                dc.b %00111111
                dc.b %01111111
                dc.b %11111111

physicalsprtbl1:dc.b 0,1,2,3,4,5,6,7            ;Indexes to frame & color
                dc.b 0,1,2,3,4,5,6,7            ;registers
                dc.b 0,1,2,3,4,5,6,7
                dc.b 0,1,2,3,4,5,6,7
                dc.b 0,1,2,3,4,5,6,7
                dc.b 0,1,2,3,4,5,6,7
                dc.b 0,1,2,3,4,5,6,7
                dc.b 0,1,2,3,4,5,6,7

physicalsprtbl2:dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14

andtbl:         dc.b 255-1
ortbl:          dc.b 1
                dc.b 255-2
                dc.b 2
                dc.b 255-4
                dc.b 4
                dc.b 255-8
                dc.b 8
                dc.b 255-16
                dc.b 16
                dc.b 255-32
                dc.b 32
                dc.b 255-64
                dc.b 64
                dc.b 255-128
                dc.b 128
    END ASM
END SUB

_RotX:
DATA AS BYTE 11,13,15,17,18,19,20,21
DATA AS BYTE 11,13,15,17,18,19,20,21
DATA AS BYTE 11,13,15,17,17,18,19,20
DATA AS BYTE 11,13,14,16,17,18,18,19
DATA AS BYTE 11,12,14,15,16,17,17,18
DATA AS BYTE 11,12,13,14,15,15,16,17
DATA AS BYTE 11,12,13,13,14,14,14,15
DATA AS BYTE 11,11,12,12,12,13,13,13
DATA AS BYTE 11,11,11,11,11,11,11,11
DATA AS BYTE 11,11,10,10,10,9,9,9
DATA AS BYTE 11,10,9,9,8,8,8,7
DATA AS BYTE 11,10,9,8,7,7,6,5
DATA AS BYTE 11,10,8,7,6,5,5,4
DATA AS BYTE 11,9,8,6,5,4,4,3
DATA AS BYTE 11,9,7,5,5,4,3,2
DATA AS BYTE 11,9,7,5,4,3,2,1
DATA AS BYTE 11,9,7,5,4,3,2,1
DATA AS BYTE 11,9,7,5,4,3,2,1
DATA AS BYTE 11,9,7,5,5,4,3,2
DATA AS BYTE 11,9,8,6,5,4,4,3
DATA AS BYTE 11,10,8,7,6,5,5,4
DATA AS BYTE 11,10,9,8,7,7,6,5
DATA AS BYTE 11,10,9,9,8,8,8,7
DATA AS BYTE 11,11,10,10,10,9,9,9
DATA AS BYTE 11,11,11,11,11,11,11,11
DATA AS BYTE 11,11,12,12,12,13,13,13
DATA AS BYTE 11,12,13,13,14,14,14,15
DATA AS BYTE 11,12,13,14,15,15,16,17
DATA AS BYTE 11,12,14,15,16,17,17,18
DATA AS BYTE 11,13,14,16,17,18,18,19
DATA AS BYTE 11,13,15,17,17,18,19,20
DATA AS BYTE 11,13,15,17,18,19,20,21
_RotY:
DATA AS BYTE 10,10,10,10,10,10,10,10
DATA AS BYTE 10,10,9,9,9,8,8,8
DATA AS BYTE 10,9,8,8,7,7,7,6
DATA AS BYTE 10,9,8,7,6,6,5,4
DATA AS BYTE 10,9,7,6,5,4,4,3
DATA AS BYTE 10,8,7,5,4,3,3,2
DATA AS BYTE 10,8,6,4,4,3,2,1
DATA AS BYTE 10,8,6,4,3,2,1,0
DATA AS BYTE 10,8,6,4,3,2,1,0
DATA AS BYTE 10,8,6,4,3,2,1,0
DATA AS BYTE 10,8,6,4,4,3,2,1
DATA AS BYTE 10,8,7,5,4,3,3,2
DATA AS BYTE 10,9,7,6,5,4,4,3
DATA AS BYTE 10,9,8,7,6,6,5,4
DATA AS BYTE 10,9,8,8,7,7,7,6
DATA AS BYTE 10,10,9,9,9,8,8,8
DATA AS BYTE 10,10,10,10,10,10,10,10
DATA AS BYTE 10,10,11,11,11,12,12,12
DATA AS BYTE 10,11,12,12,13,13,13,14
DATA AS BYTE 10,11,12,13,14,14,15,16
DATA AS BYTE 10,11,13,14,15,16,16,17
DATA AS BYTE 10,12,13,15,16,17,17,18
DATA AS BYTE 10,12,14,16,16,17,18,19
DATA AS BYTE 10,12,14,16,17,18,19,20
DATA AS BYTE 10,12,14,16,17,18,19,20
DATA AS BYTE 10,12,14,16,17,18,19,20
DATA AS BYTE 10,12,14,16,16,17,18,19
DATA AS BYTE 10,12,13,15,16,17,17,18
DATA AS BYTE 10,11,13,14,15,16,16,17
DATA AS BYTE 10,11,12,13,14,14,15,16
DATA AS BYTE 10,11,12,12,13,13,13,14
DATA AS BYTE 10,10,11,11,11,12,12,12
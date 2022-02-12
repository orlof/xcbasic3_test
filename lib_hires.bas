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
const SPR_COLOR0        = $d027
const SPR_COLOR1        = $d028
const SPR_COLOR2        = $d029
const SPR_COLOR3        = $d02a
const SPR_COLOR4        = $d02b
const SPR_COLOR5        = $d02c
const SPR_COLOR6        = $d02d
const SPR_COLOR7        = $d02e

DIM Bank(2) AS BYTE

DIM bank_addr(2) AS WORD
DIM bitmap_addr(2) AS WORD
DIM screen_addr(2) AS WORD

DIM Y_Table_Hi(200) AS BYTE
DIM Y_Table_Lo(200) AS BYTE

DIM X_Table(200) AS BYTE
DIM BitMask(200) AS BYTE
DIM DrawBuf AS BYTE SHARED


const SPRSTART          = 16

DIM sprite_shapes AS WORD: sprite_shapes = screen_addr + 1016
DIM sprite_length AS BYTE: sprite_length = 0




rem DIM BIT_SET(8) AS BYTE @ BIT_SET_MASK
rem BIT_SET_MASK:
rem DATA AS BYTE %00000001, %00000010, %00000100, %00001000, %00010000, %00100000, %01000000, %10000000

rem DIM BIT_CLR(8) AS BYTE @ BIT_CLR_MASK
rem BIT_CLR_MASK:
rem DATA AS BYTE %11111110, %11111101, %11111011, %11110111, %11101111, %11011111, %10111111, %01111111

rem **************************************
rem * Command
rem * hires_clear
rem *
rem * Clear bitmap
rem **************************************
SUB hires_clear() SHARED STATIC
  MEMSET bitmap_addr(DrawBuf), 8000, 0
END SUB

rem **************************************
rem * Command
rem * hires_on
rem *
rem * Turns on bitmap screen mode 
rem **************************************
SUB hires_on(Bank0 AS BYTE, Bank1 AS BYTE, BitMap AS BYTE, ScrMem AS BYTE) SHARED STATIC
    Bank(0) = Bank0 XOR %11
    Bank(1) = Bank1 XOR %11

    bank_addr(0) = 16384 * Bank0
    bank_addr(1) = 16384 * Bank1

    bitmap_addr(0) = bank_addr(0) + 8192 * BitMap
    bitmap_addr(1) = bank_addr(1) + 8192 * BitMap

    screen_addr(0) = bank_addr(0) + 1024 * ScrMem
    screen_addr(1) = bank_addr(1) + 1024 * ScrMem

    FOR t AS BYTE = 0 TO 199
        DIM addr AS WORD
        addr = bitmap_addr(0) + t MOD 8 + CWORD(320) * (t / 8)
        Y_Table_Lo(t) = PEEK(@addr)
        Y_Table_Hi(t) = PEEK(@addr+1)
    NEXT t

    FOR t AS BYTE = 0 TO 192 STEP 8
        FOR t2 AS BYTE = 0 TO 7
            X_Table(t+t2) = t
            BitMask(t+t2) = SHR($80, t2)
        NEXT t2
    NEXT t

    rem -- BANK 0 to 3
    DrawBuf = 0
    CALL hires_clear()
    poke $dd00, (peek($dd00) AND %11111100) OR Bank(DrawBuf)
    DrawBuf = 1
    CALL hires_clear()

    rem -- BITMAP 0 to 1, SCRMEM 0 to 15
    poke $d018, SHL(ScrMem, 4) OR SHL(BitMap, 3)

    rem -- Bitmap mode on
    poke $d011, peek($d011) OR %00100000

    rem -- Multicolor mode off
    poke $d016, peek($d016) AND %11101111
END SUB

SUB hires_swap() SHARED STATIC
    WAIT 53265, 128
    poke $dd00, (peek($dd00) AND %11111100) OR Bank(DrawBuf)
    DrawBuf = DrawBuf XOR 1
END SUB

rem **************************************
rem * Command
rem * hires_off
rem *
rem * Turns off bitmap screen mode
rem **************************************
SUB hires_off() SHARED STATIC
  rem -- Bitmap mode off
  poke $d011, peek($d011) AND %11011111
  rem -- Restore screen address to default
  poke $d018, %00010101
  rem -- Switch VIC to bank 0
  poke $dd00, peek($dd00) OR %00000011
END SUB

rem **************************************
rem * Command
rem * hires_setcolor
rem *
rem * Sets ink and background color
rem * Arguments:
rem * inkcol - Ink color (0-15)
rem * bgcol  - Background color (0-15)
rem **************************************
SUB hires_setcolor(inkcol AS BYTE, bgcol AS BYTE) SHARED STATIC
    MEMSET screen_addr(0), 1000, SHL(inkcol, 4) OR bgcol
    MEMSET screen_addr(1), 1000, SHL(inkcol, 4) OR bgcol
END SUB

rem **************************************
rem * Command
rem * hires_draw
rem * Draws a single dot
rem * 
rem * Arguments:
rem * x, y - Dot coordinates
rem **************************************
SUB hires_set(x AS BYTE, y AS BYTE) SHARED STATIC
    ASM
        ldx {x}
        cpx #200
        bcs hires_set_end

        ldy {y}
        cpy #200
        bcs hires_set_end

        lda {Y_Table_Hi},y
        sta $fc
        lda {Y_Table_Lo},y
        sta $fb

        ldy {X_Table},x
        lda {BitMask},x

        ; ===== 20 CYCLES TO NOT OVERWRITE OTHER PIXELS IN THE SAME BYTE
        ; sei
        ; dec 1
        ;     ora ($fb),y
        ; inc 1
        ; cli
        ; ===== 20 CYCLES END HERE

        sta ($fb),y
hires_set_end
    END ASM
END SUB

SUB hires_clr(x AS BYTE, y AS BYTE) SHARED STATIC
    ASM
        ldx {x}
        cpx #200
        bcs hires_clr_end

        ldy {y}
        cpy #200
        bcs hires_clr_end

        lda {Y_Table_Hi},y
        sta $fc
        lda {Y_Table_Lo},y
        sta $fb

        ldy {X_Table},x
        lda {BitMask},x
        eor #$ff

        ; ===== 20 CYCLES TO NOT OVERWRITE OTHER PIXELS IN THE SAME BYTE
        ; sei
        ; dec 1
        ;    and ($fb),y
        ; inc 1
        ; cli
        ; ===== 20 CYCLES END HERE

        sta ($fb),y
hires_clr_end
    END ASM
END SUB

rem ******************************
rem * Command
rem * hires_line
rem * Draws a line
rem * 
rem * Arguments:
rem * x1, y1 - Start coordinates
rem * x2, y2 - End coordinates
rem ******************************
SUB hires_line(x1 AS BYTE, y1 AS BYTE, x2 AS BYTE, y2 AS BYTE, mode AS BYTE) SHARED STATIC
    DIM bresenham_x1 AS BYTE FAST
    DIM bresenham_y1 AS BYTE FAST
    DIM bresenham_x2 AS BYTE FAST
    DIM bresenham_y2 AS BYTE FAST
    DIM bresenham_sx AS BYTE FAST
    DIM bresenham_sy AS BYTE FAST
    DIM bresenham_dx AS BYTE FAST
    DIM bresenham_dy AS BYTE FAST
    DIM bresenham_err AS BYTE FAST
    DIM bresenham_mode AS BYTE FAST
    ASM
        lda {mode}
        sta {bresenham_mode}
        lda {x1}
        cmp #200
        bcs bresenham_out
        sta {bresenham_x1}
        lda {x2}
        cmp #200
        bcs bresenham_out
        sta {bresenham_x2}
        lda {y2}
        cmp #200
        bcs bresenham_out
        sta {bresenham_y2}
        lda {y1}
        cmp #200
        bcs bresenham_out
        sta {bresenham_y1}
        jmp bresenham_init

bresenham_out
        jmp bresenham_end2

bresenham_init
        ldx #$ff
        ; lda {bresenham_y1}
        sec
        sbc {bresenham_y2}
        bpl bresenham_0
        ldx #1
        eor #$ff                ; neg
        clc
        adc #1
bresenham_0
        sta {bresenham_dy}
        stx {bresenham_sy}

        ldx #$ff
        lda {bresenham_x1}
        sec
        sbc {bresenham_x2}
        bpl bresenham_1
        ldx #1
        eor #$ff                ; neg
        clc
        adc #1
bresenham_1
        sta {bresenham_dx}
        stx {bresenham_sx}

        cmp {bresenham_dy}
        beq bresenham_2
        bpl bresenham_skiperr
bresenham_2
        lda {bresenham_dy}
        eor #$ff                ; neg
        clc
        adc #1
bresenham_skiperr	
        sta {bresenham_err}

        asl {bresenham_dx}
        asl {bresenham_dy}

        sei
        dec 1

bresenham_loop
        ; plot
        ldy {bresenham_y1}
        
        lda {Y_Table_Hi},y
        ldx {DrawBuf}
        beq bresenham_9

        eor #%01000000

bresenham_9
        sta $fc
        lda {Y_Table_Lo},y
        sta $fb

        ldx {bresenham_x1}
        ldy {X_Table},x
        lda {BitMask},x
        ldx {bresenham_mode}
        bne bresenham_5

        eor #$ff
        and ($fb),y
        .BYTE $2c

bresenham_5
        ora ($fb),y
        sta ($fb),y
        ; x1 != x2 ?
        lda {bresenham_x1}
        cmp {bresenham_x2}
        bne bresenham_step

        ; y1 != y2 ?
        lda {bresenham_y1}
        cmp {bresenham_y2}
        bne bresenham_step

        jmp bresenham_end

bresenham_step
        lda {bresenham_err}
        pha

        clc
        adc {bresenham_dx}
        bmi bresenham_3
        beq bresenham_3

        lda {bresenham_err}
        sec
        sbc {bresenham_dy}
        sta {bresenham_err}

        lda {bresenham_x1}
        clc
        adc {bresenham_sx}
        sta {bresenham_x1}

bresenham_3
        pla
        cmp {bresenham_dy}
        bpl bresenham_4
        
        lda {bresenham_err}
        clc
        adc {bresenham_dx}
        sta {bresenham_err}
        
        lda {bresenham_y1}
        clc
        adc {bresenham_sy}
        sta {bresenham_y1}

bresenham_4
        jmp bresenham_loop

bresenham_end
        inc 1
        cli

bresenham_end2
    END ASM
END SUB

SUB hires_block(x AS BYTE, y AS BYTE, radius AS BYTE) SHARED STATIC
    DIM x0 AS BYTE
    DIM y0 AS BYTE
    DIM x1 AS BYTE
    DIM y1 AS BYTE

    if x>radius and x+radius<200 and y > radius and y+radius < 200 THEN
        x0 = x-radius
        x1 = x+radius
        y0 = y-radius
        y1 = y+radius        
    
        DIM size AS BYTE: size = x1 - x0 + 8
        DIM addr AS WORD: addr = SHL(CWORD(Y_Table_Hi(y0)), 8) + Y_Table_Lo(y0) + x0
        IF DrawBuf=1 THEN addr = addr XOR $4000

        FOR y = y0 TO y1 STEP 8
            MEMSET addr, size, 0
            addr = addr + 320
        NEXT y
    END IF
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

SUB sprite_add_shape(src AS WORD, len AS BYTE) SHARED STATIC
    ASM
        sei
        dec 1
        dec 1
    END ASM
    MEMSHIFT src, bank_addr + 64 * CWORD(SPRSTART + sprite_length), 64 * CWORD(len)
    ASM
        inc 1
        inc 1
        cli
    END ASM
    sprite_length = sprite_length + len
END SUB

SUB sprite_on(spr_nr AS BYTE) SHARED STATIC
    POKE SPR_ENABLE, PEEK(SPR_MULTICOLOR) OR BIT_SET(spr_nr)
END SUB

SUB sprite_off(spr_nr AS BYTE) SHARED STATIC
    POKE SPR_ENABLE, PEEK(SPR_MULTICOLOR) AND BIT_CLR(spr_nr)
END SUB

SUB sprite_enable_all(value AS BYTE) SHARED STATIC
    POKE SPR_ENABLE, value
END SUB

SUB sprite_at(spr_nr AS BYTE, x AS BYTE, y AS BYTE) SHARED STATIC
    spr_nr = SHL(spr_nr, 1)
    
    POKE SPR_X_COORD + spr_nr, x + 12
    POKE SPR_Y_COORD + spr_nr, y + 40
END SUB

SUB sprite_color(spr_nr AS BYTE, color AS BYTE) SHARED STATIC
    POKE SPR_COLOR0 + spr_nr, color
END SUB

SUB sprite_hires(spr_nr AS BYTE) SHARED STATIC
    POKE SPR_MULTICOLOR, PEEK(SPR_MULTICOLOR) AND BIT_CLR(spr_nr)
END SUB

SUB sprite_multi(spr_nr AS BYTE) SHARED STATIC
    POKE SPR_MULTICOLOR, PEEK(SPR_MULTICOLOR) OR BIT_SET(spr_nr)
END SUB

SUB sprite_under_background(spr_nr AS BYTE) SHARED STATIC
    POKE SPR_DATA_PRIO, PEEK(SPR_DATA_PRIO) OR BIT_SET(spr_nr)
END SUB

SUB sprite_on_background(spr_nr AS BYTE) SHARED STATIC
    POKE SPR_DATA_PRIO, PEEK(SPR_DATA_PRIO) AND BIT_CLR(spr_nr)
END SUB

SUB sprite_shape(spr_nr AS BYTE, mem_block AS BYTE) SHARED STATIC
    POKE sprite_shapes + spr_nr, SPRSTART + mem_block
END SUB

SUB sprite_xysize(spr_nr AS BYTE, xs AS BYTE, xy AS BYTE) SHARED STATIC
    POKE SPR_EXP_X, PEEK(SPR_EXP_X) OR BIT_SET(spr_nr)
    POKE SPR_EXP_Y, PEEK(SPR_EXP_Y) AND BIT_CLR(spr_nr)
END SUB

SUB sprite_multicolor(c1 AS BYTE, c2 AS BYTE) SHARED STATIC
    POKE SPR_MCOLOR1, c1
    POKE SPR_MCOLOR2, c2    
END SUB

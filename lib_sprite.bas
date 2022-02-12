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

CONST BANK              = 2
CONST BITMAP            = 1
CONST SCREEN            = 0
CONST SPRDATA           = 16

DIM BankAddr AS WORD: BankAddr = 16384 * (BANK XOR %11)
DIM ScrMemAddr AS WORD: ScrMemAddr = BankAddr + 1024 * SCRMEM

DIM SprPtr AS WORD: SprPtr = ScrMemAddr + 1016
DIM SprNum AS BYTE: SprNum = 0

DIM PixelMask(8) AS BYTE @ _PixelMask
_PixelMask:
DATA AS BYTE %10000000, %01000000, %00100000, %00010000
DATA AS BYTE %00001000, %00000100, %00000010, %00000001

DIM OrMask(8) AS BYTE @ _OrMask
_OrMask:
DATA AS BYTE %00000001, %00000010, %00000100, %00001000, %00010000, %00100000, %01000000, %10000000

DIM AndMask(8) AS BYTE @ _AndMask
_AndMask:
DATA AS BYTE %11111110, %11111101, %11111011, %11110111, %11101111, %11011111, %10111111, %01111111



rem ******************************
rem * Command
rem * hires_line
rem * Draws a line
rem * 
rem * Arguments:
rem * x1, y1 - Start coordinates
rem * x2, y2 - End coordinates
rem ******************************
SUB spr_line(x1 AS BYTE, y1 AS BYTE, x2 AS BYTE, y2 AS BYTE) SHARED STATIC
    DIM bresenham_x1 AS BYTE FAST
    DIM bresenham_y1 AS BYTE FAST
    DIM bresenham_x2 AS BYTE FAST
    DIM bresenham_y2 AS BYTE FAST
    DIM bresenham_sx AS BYTE FAST
    DIM bresenham_sy AS BYTE FAST
    DIM bresenham_dx AS BYTE FAST
    DIM bresenham_dy AS BYTE FAST
    DIM bresenham_err AS BYTE FAST
    ASM
        ldx #$ff                ; calc dy, sy
        lda bresenham_y1
        sec
        sbc bresenham_y2
        bpl bresenham_dy_pos
        ldx #1
        eor #$ff                ; neg
        clc
        adc #1
bresenham_dy_pos
        sta bresenham_dy
        stx bresenham_sy

        ldx #$ff                ; calc dx, sx
        lda bresenham_x1
        sec
        sbc bresenham_x2
        bpl bresenham_dx_pos
        ldx #1
        eor #$ff                ; neg
        clc
        adc #1
bresenham_dx_pos
        sta bresenham_dx
        stx bresenham_sx

        cmp bresenham_dy
        beq bresenham_err_dy
        bpl bresenham_err_dx
bresenham_err_dy
        lda bresenham_dy
        eor #$ff                ; neg
        clc
        adc #1
bresenham_err_dx
        sta bresenham_err

        asl bresenham_dx
        asl bresenham_dy

bresenham_loop
        ; plot
        lda bresenham_x1	; addr offset to y
        lsr
        lsr
        lsr
        clc
        adc bresenham_y1
        adc bresenham_y1
        adc bresenham_y1
        tay
        
	    lda bresenham_x1	; bitmask offset to x
        and #%00000111
        tax
        
        lda ($fb),y
        ora pixel,x
        sta ($fb),y
        
        ; x1 != x2 ?
        lda bresenham_x1
        cmp bresenham_x2
        bne bresenham_step

        ; y1 != y2 ?
        lda bresenham_y1
        cmp bresenham_y2
        bne bresenham_step

        jmp bresenham_end

bresenham_step
        lda bresenham_err
        pha

        clc
        adc bresenham_dx
        bmi bresenham_no_dx
        beq bresenham_no_dx

        lda bresenham_err
        sec
        sbc bresenham_dy
        sta bresenham_err
        
        lda bresenham_x1
        clc
        adc bresenham_sx
        sta bresenham_x1

bresenham_no_dx
        pla
        cmp bresenham_dy
        bpl bresenham_no_dy
        
        lda bresenham_err
        clc
        adc bresenham_dx
        sta bresenham_err
        
        lda bresenham_y1
        clc
        adc bresenham_sy
        sta bresenham_y1

bresenham_no_dy
        jmp bresenham_loop
bresenham_end
    END ASM
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

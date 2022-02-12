include "lib_gfx.bas"

FOR w AS WORD = 0 TO 63
    REM POKE CWORD($8400) + w, $ff
    POKE CWORD($3000) + w, $ff
    POKE CWORD(33792) + w, $ff
NEXT w

FOR t AS BYTE = 0 TO 15
    CALL SpriteColor(t, t)
    CALL SpriteAt(t, 16*t, 16*t)
    CALL SpriteShape(t, 16)
    REM CALL SpriteShape(t, 192)
NEXT t

CALL hires_on(2, 1, 0)
CALL hires_clear()
CALL hires_color(COLOR_WHITE, COLOR_BLACK)

CALL SpriteInit()

rem FOR x AS BYTE = 0 TO 255
looper:
    FOR t AS BYTE = 0 TO 15
        CALL SpriteMove(t, 1, 1)
    NEXT t
    CALL SpriteUpdate()
goto looper
rem NEXT x

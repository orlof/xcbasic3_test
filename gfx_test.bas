include "lib_gfx.bas"

CALL hires_on(2, 1, 0)
CALL hires_clear()
CALL hires_color(COLOR_WHITE, COLOR_BLACK)

FOR t AS BYTE = 0 TO 15
    CALL SpriteColor(t, t)
    CALL SpriteAt(t, 16*t, 16*t)
    CALL SpriteShape(t, 16)
    REM CALL SpriteShape(t, 192)
NEXT t

CALL SpriteClear(16)
CALL SpriteLine(16, 0, 0, 23, 20)
CALL SpriteLine(16, 0, 20, 23, 0)
CALL SpriteLine(16, 0, 0, 23, 0)
CALL SpriteLine(16, 0, 20, 23, 20)
CALL SpriteLine(16, 0, 0, 0, 20)
CALL SpriteLine(16, 23, 0, 23, 20)

CALL SpriteInit()

rem FOR x AS BYTE = 0 TO 255
looper:
    FOR t AS BYTE = 0 TO 15
        CALL SpriteMove(t, 1, 1)
    NEXT t
    CALL SpriteUpdate()
goto looper
rem NEXT x

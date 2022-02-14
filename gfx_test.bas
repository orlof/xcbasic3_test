include "lib_gfx.bas"

TYPE Vertice
    Angular AS BYTE
    Radial AS BYTE
END TYPE

CONST SHAPE_END  = %10000000

DIM Geometry(100) AS Vertice @ _Geometry
    FOR Index AS BYTE = 0 TO 255
        IF (Geometry(Index).Radial AND SHAPE_END) <> 0 THEN EXIT FOR
        Geometry(Index).Angular = SHL(Geometry(Index).Angular, 3)
    NEXT Index

CALL hires_on(2, 1, 0)
CALL hires_clear()
CALL hires_color(COLOR_WHITE, COLOR_BLACK)

FOR t AS BYTE = 0 TO 15
    CALL SpriteColor(t, t)
    CALL SpriteAt(t, 16*t, 16*t)
    CALL SpriteShape(t, 16+t)
    CALL ShapeClear(16+t)
    CALL ShapeDrawGeometry(t+16, @GeomShip, 16*t)
NEXT t

CALL SpriteInit()
DIM dir(16) AS BYTE
    FOR t AS BYTE = 0 TO 15
        dir(t) = t * 8
    NEXT t

looper:
    FOR t AS BYTE = 0 TO 15
        dir(t) = dir(t) + 8
        CALL ShapeClear((t AND 15) + 16)
        CALL ShapeDrawGeometry((t AND 15) + 16, @GeomShip, dir(t))
        CALL SpriteMove(t, 1, 1)
        CALL SpriteUpdate()
    NEXT t
goto looper

REM Radial: 0 to 7 -> [0, 2, 4, 6, 8, 10, 12, 15]
REM Angular: 0 to 31 -> 0 = Right, 8 = Up, 16 = Left, 24 = Down (11.6 deg / step)
REM ship = 0

_Geometry:
GeomShip:
DATA AS BYTE 8, 3
DATA AS BYTE 20, 3
DATA AS BYTE 28, 3
DATA AS BYTE 8, 3
DATA AS BYTE 0, %01000000
GeomUfo:
DATA AS BYTE 0, 5
DATA AS BYTE 4, 3
DATA AS BYTE 8, 5
DATA AS BYTE 12, 3
DATA AS BYTE 16, 5
DATA AS BYTE 20, 3
DATA AS BYTE 24, 5
DATA AS BYTE 28, 3
DATA AS BYTE 0, 5
DATA AS BYTE 0, %01000000
DATA AS BYTE 0, %10000000

include "lib_gfx.bas"

TYPE Vertice
    Angular AS BYTE
    Radial AS BYTE
END TYPE

TYPE Ship
    angle AS BYTE
    rotation AS BYTE
    speed AS BYTE
    geometry AS WORD
    slot AS BYTE
END TYPE

CONST SHAPE_END  = %10000000

DIM Geometry(255) AS Vertice @ _Geometry
    FOR Index AS BYTE = 0 TO 255
        IF (Geometry(Index).Radial AND SHAPE_END) <> 0 THEN EXIT FOR
        Geometry(Index).Angular = SHL(Geometry(Index).Angular, 3)
    NEXT Index

CALL hires_on(2, 1, 0)
CALL hires_clear()
CALL hires_color(COLOR_WHITE, COLOR_BLACK)
CALL hires_unset()

DIM shape(16) AS WORD
    shape(0) = @GeomShip0
    shape(1) = @GeomShip1
    shape(2) = @GeomShip2
    shape(3) = @GeomShip3
    shape(4) = @GeomShip4
    shape(5) = @GeomShip5
    shape(6) = @GeomShip6
    shape(7) = @GeomShip7
    shape(8) = @GeomShip8
    shape(9) = @GeomShip9
    shape(10) = @GeomShip10
    shape(11) = @GeomShip11
    shape(12) = @GeomShip12
    shape(13) = @GeomShip13
    shape(14) = @GeomShip14
    shape(15) = @GeomShip15

DIM t AS BYTE
DIM s AS BYTE

DIM Ships(16) AS Ship
    FOR t = 0 TO 15
        Ships(t).angle = 16 * t
        Ships(t).speed = (t AND 3) + 1
        Ships(t).rotation = t-8
        Ships(t).geometry = shape(t)
        Ships(t).slot = 32 + t
        
        IF t=0 THEN 
            CALL SpriteColor(t, 1)
        ELSE
            CALL SpriteColor(t, t)
        END IF

        CALL SpriteAt(t, 16*t, 16*t)
        CALL SpriteShape(t, Ships(t).slot)
        CALL ShapeClear(Ships(t).slot)
        CALL ShapeDrawGeometry(Ships(t).slot, Ships(t).geometry, Ships(t).angle)
    NEXT t

CALL SpriteInit()

TYPE Pixel
    x AS BYTE
    y AS BYTE
    dx AS BYTE
    dy AS BYTE
END TYPE

DIM Ammo(15) AS Pixel

looper:
    FOR s = 0 TO 15
        Ammo(s).x = sprx(s) - 24 + 11
        Ammo(s).y = spry(s) - 50 + 10
        index = (Ships(s).angle AND %11111000) OR 1
        Ammo(s).dx = RotX(index) - 11
        Ammo(s).dy = RotY(index) - 10
        index = (Ships(s).angle AND %11111000) OR Ships(s).speed
        Ammo(s).dx = Ammo(s).dx + RotX(index) - 11
        Ammo(s).dy = Ammo(s).dy + RotY(index) - 10
    NEXT s
    FOR s = 0 TO 50
        CALL hires_unset()
        FOR t = 0 TO 15
            Ammo(t).x = Ammo(t).x + Ammo(t).dx
            Ammo(t).y = Ammo(t).y + Ammo(t).dy
            CALL hires_set(Ammo(t).x, Ammo(t).y)
        NEXT t
        FOR t = 0 TO 15            
            DIM PrevAngle AS BYTE: PrevAngle = Ships(t).angle
            Ships(t).angle = Ships(t).angle + Ships(t).rotation
            IF ((Ships(t).angle XOR PrevAngle) AND %11111000) <> 0 THEN
                Ships(t).slot = Ships(t).slot XOR %00010000
                CALL ShapeClear(Ships(t).slot)
                CALL ShapeDrawGeometry(Ships(t).slot, Ships(t).geometry, Ships(t).angle)
                CALL SpriteShape(t, Ships(t).slot)
            END IF
            CALL SpriteMoveForward(t, Ships(t).angle, Ships(t).speed)
        NEXT t
        CALL SpriteUpdate()
    NEXT s
goto looper

REM Radial: 0 to 7 -> [0, 2, 4, 6, 7, 8, 9, 10]
REM Angular: 0 to 31 -> 0 = Right, 8 = Up, 16 = Left, 24 = Down (11.6 deg / step)
REM ship = 0

_Geometry:
GeomShip0:
DATA AS BYTE 0, 3
DATA AS BYTE 20, 3
DATA AS BYTE 12, 3
DATA AS BYTE 0, 3
DATA AS BYTE 0, %01000000
GeomShip1:
DATA AS BYTE 0, 7
DATA AS BYTE 4, 2
DATA AS BYTE 8, 7
DATA AS BYTE 12, 2
DATA AS BYTE 20, 2
DATA AS BYTE 24, 7
DATA AS BYTE 28, 2
DATA AS BYTE 0, 7
DATA AS BYTE 0, %01000000
GeomShip2:
DATA AS BYTE 6, 7
DATA AS BYTE 10, 7
DATA AS BYTE 0, 5
DATA AS BYTE 22, 7
DATA AS BYTE 26, 7
DATA AS BYTE 22, %00100111
DATA AS BYTE 10, 7
DATA AS BYTE 0, %01000000
GeomShip3:
DATA AS BYTE 6, 7
DATA AS BYTE 10, 7
DATA AS BYTE 4, 2
DATA AS BYTE 0, 7
DATA AS BYTE 28, 2
DATA AS BYTE 22, 7
DATA AS BYTE 26, 7
DATA AS BYTE 22, %00100111
DATA AS BYTE 10, 7
DATA AS BYTE 0, %01000000
GeomShip4:
DATA AS BYTE 0, 7
DATA AS BYTE 12, 7
DATA AS BYTE 20, 7
DATA AS BYTE 0, 7
DATA AS BYTE 0, %01000000
GeomShip5:
DATA AS BYTE 0, 7
DATA AS BYTE 4, 3
DATA AS BYTE 8, 7
DATA AS BYTE 12, 7
DATA AS BYTE 16, 1
DATA AS BYTE 20, 7
DATA AS BYTE 24, 7
DATA AS BYTE 28, 3
DATA AS BYTE 0, 7
DATA AS BYTE 0, %01000000
GeomShip6:
DATA AS BYTE 0, 7
DATA AS BYTE 4, 4
DATA AS BYTE 8, 7
DATA AS BYTE 0, 2
DATA AS BYTE 24, 7
DATA AS BYTE 28, 4
DATA AS BYTE 0, 7
DATA AS BYTE 0, %01000000
GeomShip7:
DATA AS BYTE 0, 7
DATA AS BYTE 12, 7
DATA AS BYTE 16, 0
DATA AS BYTE 20, 7
DATA AS BYTE 0, 7
DATA AS BYTE 0, %01000000
GeomShip8:
DATA AS BYTE 0, 7
DATA AS BYTE 12, 7
DATA AS BYTE 20, 7
DATA AS BYTE 0, 7
DATA AS BYTE 0, %00100010
DATA AS BYTE 12, 2
DATA AS BYTE 20, 2
DATA AS BYTE 0, 2
DATA AS BYTE 0, %01000000
GeomShip9:
DATA AS BYTE 0, 4
DATA AS BYTE 18, 6
DATA AS BYTE 14, 6
DATA AS BYTE 0, 4
DATA AS BYTE 0, 7
DATA AS BYTE 0, %01000000
GeomShip10:
DATA AS BYTE 0, 5
DATA AS BYTE 12, 5
DATA AS BYTE 20, 5
DATA AS BYTE 0, 5
DATA AS BYTE 12, %00100101
DATA AS BYTE 14, 7
DATA AS BYTE 16, 3
DATA AS BYTE 18, 7
DATA AS BYTE 20, 5
DATA AS BYTE 0, %01000000
GeomShip11:
DATA AS BYTE 0, 6
DATA AS BYTE 30, 2
DATA AS BYTE 28, 6
DATA AS BYTE 19, 3
DATA AS BYTE 16, 1
DATA AS BYTE 13, 3
DATA AS BYTE 4, 6
DATA AS BYTE 2, 2
DATA AS BYTE 0, 6
DATA AS BYTE 0, %01000000
GeomShip12:
DATA AS BYTE 0, 7
DATA AS BYTE 26, 4
DATA AS BYTE 22, 4
DATA AS BYTE 10, 4
DATA AS BYTE 6, 4
DATA AS BYTE 0, 7
DATA AS BYTE 0, %01000000
GeomShip13:
DATA AS BYTE 0, 7
DATA AS BYTE 30, 5
DATA AS BYTE 18, 5
DATA AS BYTE 14, 5
DATA AS BYTE 2, 5
DATA AS BYTE 0, 7
DATA AS BYTE 0, %01000000
GeomShip14:
DATA AS BYTE 28, 4
DATA AS BYTE 20, 5
DATA AS BYTE 4, 4
DATA AS BYTE 12, 5
DATA AS BYTE 28, 4
DATA AS BYTE 0, 7
DATA AS BYTE 4, 4
DATA AS BYTE 0, %01000000
GeomShip15:
DATA AS BYTE 0, 6
DATA AS BYTE 25, 5
DATA AS BYTE 20, 6
DATA AS BYTE 12, 6
DATA AS BYTE 7, 5
DATA AS BYTE 0, 6
DATA AS BYTE 0, %00100011
DATA AS BYTE 20, 3
DATA AS BYTE 12, 3
DATA AS BYTE 0, 3
DATA AS BYTE 0, %01000000

DATA AS BYTE 0, %10000000

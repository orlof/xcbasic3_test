include "lib_gfx.bas"

TYPE Vertice
    Angular AS BYTE
    Radial AS BYTE
END TYPE

TYPE Ship
    x AS WORD
    y AS WORD
    dx AS WORD
    dy AS WORD
    VisibleAngle AS BYTE
    RealAngle AS BYTE
    rotation AS BYTE
    geometry AS WORD
    slot AS BYTE
END TYPE

DIM ImpulseX(32) AS WORD @ _ImpulseX
DIM ImpulseY(32) AS WORD @ _ImpulseY

CONST SHAPE_END  = %10000000

DIM Geometry(255) AS Vertice @ _Geometry
    FOR Index AS INT = 0 TO 255
        IF (Geometry(Index).Radial AND SHAPE_END) <> 0 THEN EXIT FOR
        Geometry(Index).Angular = SHL(Geometry(Index).Angular, 3)
    NEXT Index
    print 2*Index
    end

CALL hires_setup(2, 1, 0)
CALL hires_color(COLOR_WHITE, COLOR_BLACK)
CALL hires_clear()
CALL hires_on()

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

DIM t AS BYTE FAST
DIM s AS BYTE

DIM Ships(16) AS Ship
    FOR t = 0 TO 15
        Ships(t).x = 0
        POKE @Ships(t).x + 1, 16 * t
        Ships(t).y = 0
        POKE @Ships(t).y + 1, 16 * t
        Ships(t).dx = 0
        Ships(t).dy = 0
        Ships(t).RealAngle = 16 * t
        Ships(t).VisibleAngle = Ships(t).RealAngle
        Ships(t).rotation = SGN(CINT(t)-8)
        Ships(t).geometry = shape(t)
        Ships(t).slot = 32 + t
        
        IF t=0 THEN 
            CALL SpriteColor(t, 1)
        ELSE
            CALL SpriteColor(t, t)
        END IF

        CALL SpriteAt(t, PEEK(@Ships(t).x + 1), PEEK(@Ships(t).y + 1))
        CALL SpriteShape(t, Ships(t).slot)
        CALL ShapeClear(Ships(t).slot)
        CALL ShapeDrawGeometry(Ships(t).slot, Ships(t).geometry, Ships(t).RealAngle)
    NEXT t

CALL SpriteInit()

TYPE Pixel
    x AS BYTE
    y AS BYTE
    dx AS BYTE
    dy AS BYTE
END TYPE

DIM Ammo(16) AS Pixel
DIM NumShapesUpdated AS BYTE
DIM GameLoopCounter AS BYTE
    GameLoopCounter = 0
CONST ACCELERATION_INTERVAL = 1
DIM NextAccelerationUpdate AS BYTE
    NextAccelerationUpdate = ACCELERATION_INTERVAL

looper:
    FOR t = 0 TO 15
        Ammo(t).x = sprx(t) - 24 + 11
        Ammo(t).y = spry(t) - 50 + 10
        index = (Ships(t).RealAngle AND %11111000) OR 4
        Ammo(t).dx = RotX(index) - 11
        Ammo(t).dy = RotY(index) - 10
    NEXT t
    FOR s = 0 TO 50
        CALL hires_unset()
        FOR t = 0 TO 15
            Ammo(t).x = Ammo(t).x + Ammo(t).dx
            Ammo(t).y = Ammo(t).y + Ammo(t).dy
            CALL hires_set(Ammo(t).x, Ammo(t).y)
        NEXT t
        NumShapesUpdated = 0
        FOR t = 0 TO 15            
            Ships(t).RealAngle = Ships(t).RealAngle + Ships(t).rotation
            IF NumShapesUpdated < 2 THEN
                IF ((Ships(t).RealAngle XOR Ships(t).VisibleAngle) AND %11111000) <> 0 THEN
                    NumShapesUpdated = NumShapesUpdated + 1
                    Ships(t).VisibleAngle = Ships(t).RealAngle
                    Ships(t).slot = Ships(t).slot XOR %00010000
                    CALL ShapeClear(Ships(t).slot)
                    CALL ShapeDrawGeometry(Ships(t).slot, Ships(t).geometry, Ships(t).RealAngle)
                    CALL SpriteShape(t, Ships(t).slot)
                END IF
            END IF
            CALL SpriteAt(t, PEEK(@Ships(t).x + 1), PEEK(@Ships(t).y + 1))
        NEXT t
        IF GameLoopCounter = NextAccelerationUpdate THEN
            NextAccelerationUpdate = GameLoopCounter + ACCELERATION_INTERVAL
            FOR t = 0 TO 15            
                Ships(t).dx = Ships(t).dx + ImpulseX(SHR(Ships(t).RealAngle,3))
                Ships(t).dy = Ships(t).dy + ImpulseY(SHR(Ships(t).RealAngle,3))
            NEXT t
        END IF
        FOR t = 0 TO 15            
            Ships(t).x = Ships(t).x + Ships(t).dx
            Ships(t).y = Ships(t).y + Ships(t).dy
        NEXT t
        CALL SpriteUpdate()
        GameLoopCounter = GameLoopCounter + 1
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

_ImpulseX:
DATA AS WORD 16,16,15,13,11,9,6,3
DATA AS WORD 0,65533,65530,65527,65525,65523,65521,65520
DATA AS WORD 65520,65520,65521,65523,65525,65527,65530,65533
DATA AS WORD 0,3,6,9,11,13,15,16
_ImpulseY:
DATA AS WORD 0,65533,65530,65527,65525,65523,65521,65520
DATA AS WORD 65520,65520,65521,65523,65525,65527,65530,65533
DATA AS WORD 0,3,6,9,11,13,15,16
DATA AS WORD 16,16,15,13,11,9,6,3
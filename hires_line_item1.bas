include "hires4.bas"

CONST SHAPE_END  = %10000000
CONST SHAPE_NEXT = %01000000
CONST SHAPE_SKIP = %00100000

CALL hires_on(2, 3, 1, 0)
CALL hires_setcolor(COLOR_WHITE, COLOR_BLACK)

TYPE Vertice
    Angular AS BYTE
    Radial AS BYTE
END TYPE

DIM RotX(256) AS BYTE @ _RotX
DIM RotY(256) AS BYTE @ _RotY

DIM Geometry(100) AS Vertice @ _Geometry
    FOR Index AS BYTE = 0 TO 255
        IF (Geometry(Index).Radial AND SHAPE_END) <> 0 THEN EXIT FOR
        Geometry(Index).Angular = SHL(Geometry(Index).Angular, 3)
    NEXT Index

Type Spatial
    GeometryStart AS BYTE
    MaxRadius AS BYTE
    x AS BYTE
    y AS BYTE
    Prev0X AS BYTE
    Prev0Y AS BYTE
    Prev1X AS BYTE
    Prev1Y AS BYTE
    Angle AS BYTE

    SUB init(ShapeIndex AS BYTE) STATIC
        DIM Distance AS BYTE
        DIM Radius AS BYTE
        THIS.GeometryStart = ShapeIndex
        THIS.MaxRadius = 0
        DO WHILE (Geometry(ShapeIndex).Radial AND SHAPE_NEXT) = 0
            Radius = RotX(Geometry(ShapeIndex).Radial AND %00000111)
            IF Radius > THIS.MaxRadius THEN
                THIS.MaxRadius = Radius
            END IF
            ShapeIndex = ShapeIndex + 1
        LOOP
    END SUB

    SUB erase() STATIC
        IF DrawBuf = 0 THEN
            CALL hires_block(THIS.Prev0X, THIS.Prev0Y, THIS.MaxRadius)
        ELSE
            CALL hires_block(THIS.Prev1X, THIS.Prev1Y, THIS.MaxRadius)
        END IF
    END SUB

    SUB draw() STATIC
        DIM ShapeIndex AS BYTE
            ShapeIndex = THIS.GeometryStart
        DIM x1 AS byte
        DIM y1 AS byte
        DIM x2 AS byte
        DIM y2 AS byte
        DIM Angle AS BYTE
        DIM Radius AS BYTE

        IF DrawBuf = 0 THEN
            THIS.Prev0X = THIS.x
            THIS.Prev0Y = THIS.y
        ELSE
            THIS.Prev1X = THIS.x
            THIS.Prev1Y = THIS.y
        END IF

        Angle = Geometry(ShapeIndex).Angular + THIS.Angle
        Radius = Geometry(ShapeIndex).Radial
        Angle = (Angle AND %11111000) OR (Radius AND %00000111)
        x2 = RotX(Angle) + THIS.x
        y2 = RotY(Angle) + THIS.y
        ShapeIndex = ShapeIndex + 1

        DO
            x1 = x2
            y1 = y2
            Angle = Geometry(ShapeIndex).Angular + THIS.Angle
            Radius = Geometry(ShapeIndex).Radial
            Angle = (Angle AND %11111000) OR (Radius AND %00000111)
            x2 = RotX(Angle) + THIS.x
            y2 = RotY(Angle) + THIS.y
            IF (Radius AND SHAPE_SKIP) = 0 THEN
                IF x1 < 200 AND x2 < 200 AND y1 < 200 AND y2 < 200 THEN
                    CALL hires_line(x1, y1, x2, y2, 1)
                END IF
            END IF
            ShapeIndex = ShapeIndex + 1
        LOOP WHILE (Geometry(ShapeIndex).Radial AND SHAPE_NEXT) = 0
    END SUB
End Type

CONST NUM_ITEMS = 1

RANDOMIZE TI()
DIM Item(10) AS Spatial
    FOR t AS BYTE = 0 TO NUM_ITEMS
        CALL Item(t).init(5)
        Item(t).x = RNDB()
        Item(t).y = RNDB()
        Item(t).angle = 0
    NEXT t

FOR m AS BYTE = 0 to 2
    rem draw ship
    FOR t AS BYTE = 0 TO NUM_ITEMS
        Item(t).angle = Item(t).angle + (t AND %111) - 3
        Item(t).x = Item(t).x + 1 - 2 * (t AND %1)
        Item(t).y = Item(t).y + 1 - (t AND %10)
        CALL Item(t).draw()
    NEXT t
    CALL hires_swap()
    FOR t AS BYTE = 0 TO NUM_ITEMS
        print item(t).prev0x;" ";item(t).prev1x
        CALL Item(t).erase()
    NEXT t
NEXT m
END

_Geometry:
REM Radial: 0 to 7 -> [0, 2, 4, 6, 8, 10, 12, 15]
REM Angular: 0 to 31 -> 0 = Right, 8 = Up, 16 = Left, 24 = Down (11.6 deg / step)
REM ship = 0
DATA AS BYTE 8, 3
DATA AS BYTE 20, 3
DATA AS BYTE 28, 3
DATA AS BYTE 8, 3
DATA AS BYTE 0, %01000000
REM ufo = 5
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
_RotX:
DATA AS BYTE 0,2,4,6,8,10,12,15
DATA AS BYTE 0,2,4,6,8,10,12,15
DATA AS BYTE 0,2,4,6,7,9,11,14
DATA AS BYTE 0,2,3,5,7,8,10,12
DATA AS BYTE 0,1,3,4,6,7,8,11
DATA AS BYTE 0,1,2,3,4,6,7,8
DATA AS BYTE 0,1,2,2,3,4,5,6
DATA AS BYTE 0,0,1,1,2,2,2,3
DATA AS BYTE 0,0,0,0,0,0,0,0
DATA AS BYTE 0,0,255,255,254,254,254,253
DATA AS BYTE 0,255,254,254,253,252,251,250
DATA AS BYTE 0,255,254,253,252,250,249,248
DATA AS BYTE 0,255,253,252,250,249,248,245
DATA AS BYTE 0,254,253,251,249,248,246,244
DATA AS BYTE 0,254,252,250,249,247,245,242
DATA AS BYTE 0,254,252,250,248,246,244,241
DATA AS BYTE 0,254,252,250,248,246,244,241
DATA AS BYTE 0,254,252,250,248,246,244,241
DATA AS BYTE 0,254,252,250,249,247,245,242
DATA AS BYTE 0,254,253,251,249,248,246,244
DATA AS BYTE 0,255,253,252,250,249,248,245
DATA AS BYTE 0,255,254,253,252,250,249,248
DATA AS BYTE 0,255,254,254,253,252,251,250
DATA AS BYTE 0,0,255,255,254,254,254,253
DATA AS BYTE 0,0,0,0,0,0,0,0
DATA AS BYTE 0,0,1,1,2,2,2,3
DATA AS BYTE 0,1,2,2,3,4,5,6
DATA AS BYTE 0,1,2,3,4,6,7,8
DATA AS BYTE 0,1,3,4,6,7,8,11
DATA AS BYTE 0,2,3,5,7,8,10,12
DATA AS BYTE 0,2,4,6,7,9,11,14
DATA AS BYTE 0,2,4,6,8,10,12,15
_RotY:
DATA AS BYTE 0,0,0,0,0,0,0,0
DATA AS BYTE 0,0,255,255,254,254,254,253
DATA AS BYTE 0,255,254,254,253,252,251,250
DATA AS BYTE 0,255,254,253,252,250,249,248
DATA AS BYTE 0,255,253,252,250,249,248,245
DATA AS BYTE 0,254,253,251,249,248,246,244
DATA AS BYTE 0,254,252,250,249,247,245,242
DATA AS BYTE 0,254,252,250,248,246,244,241
DATA AS BYTE 0,254,252,250,248,246,244,241
DATA AS BYTE 0,254,252,250,248,246,244,241
DATA AS BYTE 0,254,252,250,249,247,245,242
DATA AS BYTE 0,254,253,251,249,248,246,244
DATA AS BYTE 0,255,253,252,250,249,248,245
DATA AS BYTE 0,255,254,253,252,250,249,248
DATA AS BYTE 0,255,254,254,253,252,251,250
DATA AS BYTE 0,0,255,255,254,254,254,253
DATA AS BYTE 0,0,0,0,0,0,0,0
DATA AS BYTE 0,0,1,1,2,2,2,3
DATA AS BYTE 0,1,2,2,3,4,5,6
DATA AS BYTE 0,1,2,3,4,6,7,8
DATA AS BYTE 0,1,3,4,6,7,8,11
DATA AS BYTE 0,2,3,5,7,8,10,12
DATA AS BYTE 0,2,4,6,7,9,11,14
DATA AS BYTE 0,2,4,6,8,10,12,15
DATA AS BYTE 0,2,4,6,8,10,12,15
DATA AS BYTE 0,2,4,6,8,10,12,15
DATA AS BYTE 0,2,4,6,7,9,11,14
DATA AS BYTE 0,2,3,5,7,8,10,12
DATA AS BYTE 0,1,3,4,6,7,8,11
DATA AS BYTE 0,1,2,3,4,6,7,8
DATA AS BYTE 0,1,2,2,3,4,5,6
DATA AS BYTE 0,0,1,1,2,2,2,3
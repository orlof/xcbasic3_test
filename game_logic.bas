INCLUDE "hires4.bas"
INCLUDE "joy.bas"
INCLUDE "rnd.bas"

CONST NUM_STARS = 10
CONST NUM_BULLETS = 5

RANDOMIZE(ti())

CALL hires_on(2,3,1,0)
CALL hires_setcolor(COLOR_WHITE, COLOR_BLACK)
CALL hires_clear()
CALL hires_swap()
call hires_clear()

CALL sprite_add_shape(@SPRITE_DATA, 32)

TYPE impulse
    dx AS INT
    dy AS INT
END TYPE

DIM impulses(16) AS impulse @_impulses

_impulses:
DATA AS INT 0, -8, 3, -7, 6, -6, 7, -3, 8, 0, 7, 3, 6, 6, 3, 7
DATA AS INT 0, 8, -3, 7, -6, 6, -7, 3, -8, 0, -7, -3, -6, -6, -3, -7

TYPE ship
    x AS LONG
    y AS LONG
    acc_completed AS LONG
    rot_completed AS LONG
    trg_completed AS LONG
    max_speed AS INT
    vx AS INT
    vy AS INT
    bullet_ttl AS INT
    acc_delay AS BYTE
    rot_delay AS BYTE
    trg_delay AS BYTE
    d AS BYTE
    c AS BYTE
END TYPE

DIM player AS ship @_player

_player:
DATA AS LONG $006400, $006400   : rem x, y
DATA AS LONG 0, 0, 0            : rem acc_completed, rot_completed, trg_completed
DATA AS INT 128                 : rem max_speed
DATA AS INT 0, 0                : rem vx, vy
DATA AS INT 250                 : rem bullet_ttl
DATA AS BYTE 10                 : rem acc_delay
DATA AS BYTE 10                 : rem rot_delay
DATA AS BYTE 40                 : rem trg_delay
DATA AS BYTE 15                 : rem direction
DATA AS BYTE 3                  : rem color = COLOR_CYAN

TYPE projectile
    x AS LONG
    y AS LONG
    ttl AS LONG
    vx AS INT
    vy AS INT
    sx AS BYTE
    sy AS BYTE
    enabled AS BYTE
END TYPE

DIM bullets(NUM_BULLETS) AS projectile

TYPE star
    x AS BYTE
    y AS BYTE
    sx AS BYTE
    sy AS BYTE
END TYPE

DIM stars(NUM_STARS) AS star

FOR i AS BYTE = 0 TO NUM_STARS-1
    stars(i).x = rndb() AND %11111000
    stars(i).y = rndb() AND %11111000
    stars(i).sx = 255
    stars(i).sy = 255
NEXT

CALL sprite_multicolor(COLOR_LIGHTRED, COLOR_YELLOW)
CALL sprite_hires(0)
CALL sprite_multi(1)
CALL sprite_color(0, COLOR_MIDDLEGRAY)
CALL sprite_color(1, COLOR_CYAN)

CALL sprite_at(0, 100, 100)
CALL sprite_at(1, 100, 100)
CALL sprite_shape(0, 0)
CALL sprite_shape(1, 16)
CALL sprite_on(0)
CALL sprite_on(1)

DIM game_time AS LONG: game_time = 0

DIM game_over AS BYTE: game_over = 0

CONST ENABLE_STARS = 1
CONST ENABLE_PORRIDGE = 0
CONST ENABLE_BULLETS = 1

DO
    IF ENABLE_STARS=1 THEN
        FOR i AS BYTE = 0 TO NUM_STARS-1
            CALL hires_clr(stars(i).sx, stars(i).sy)
            rem TODO this used to be with peek
            stars(i).sx = stars(i).x - PEEK(@player.x + 1)
            stars(i).sy = stars(i).y - PEEK(@player.y + 1)
            CALL hires_set(stars(i).sx, stars(i).sy)
        NEXT
    END IF

    IF game_time > player.rot_completed THEN
        IF JOY_LEFT(2) OR JOY_LEFT(1) THEN
            player.d = (player.d - 1) AND $f
            player.rot_completed = game_time + player.rot_delay
        END IF
        IF JOY_RIGHT(2) OR JOY_RIGHT(1) THEN
            player.d = (player.d + 1) AND $f
            player.rot_completed = game_time + player.rot_delay
        END IF
    END IF

    IF game_time > player.acc_completed THEN
        IF JOY_UP(2) OR JOY_UP(1) THEN
            player.vx = player.vx + impulses(player.d).dx
            player.acc_completed = game_time + player.acc_delay
            IF player.vx > player.max_speed THEN
                player.vx = player.max_speed
            END IF
            IF player.vx < -player.max_speed THEN
                player.vx = -player.max_speed
            END IF
            player.vy = player.vy + impulses(player.d).dy
            IF player.vy > player.max_speed THEN
                player.vy = player.max_speed
            END IF
            IF player.vy < -player.max_speed THEN
                player.vy = -player.max_speed
            END IF
        END IF
    END IF

    IF ENABLE_PORRIDGE=1 THEN
        IF (game_time AND %00001111) = %00001111 THEN
            IF player.vx < 0 THEN
                player.vx = player.vx + 1
            ELSE
                player.vx = player.vx - 1
            END IF
            IF player.vy < 0 THEN
                player.vy = player.vy + 1
            ELSE
                player.vy = player.vy - 1
            END IF
        END IF
    END IF

    IF ENABLE_BULLETS = 1 THEN
        DIM trigger AS BYTE: trigger = 0
        IF game_time > player.trg_completed THEN
            if joy_fire(2) OR joy_fire(1) THEN
                player.trg_completed = game_time + player.trg_delay
                trigger = 1
            END IF
        END IF

        FOR t AS BYTE = 0 TO NUM_BULLETS-1
            IF bullets(t).enabled = 0 THEN
                IF trigger = 1 THEN
                    bullets(t).x = player.x
                    bullets(t).y = player.y
                    bullets(t).sx = 255
                    bullets(t).sy = 255
                    bullets(t).vx = SHL(impulses(player.d).dx, 3) + player.vx
                    bullets(t).vy = SHL(impulses(player.d).dy, 3) + player.vy
                    bullets(t).ttl = game_time + player.bullet_ttl
                    bullets(t).enabled = 1
                    trigger = 0
                END IF
            END IF

            IF bullets(t).enabled = 1 THEN
                CALL hires_clr(bullets(t).sx, bullets(t).sy)
                IF game_time > bullets(t).ttl THEN
                    bullets(t).enabled = 0
                ELSE
                    bullets(t).x = bullets(t).x + bullets(t).vx
                    bullets(t).y = bullets(t).y + bullets(t).vy
                    DIM x AS LONG: x = bullets(t).x - player.x
                    DIM y AS LONG: y = bullets(t).y - player.y
                    IF ABS(x) < $6400 AND ABS(y) < $6400 THEN
                        bullets(t).sx = 100 + PEEK(@x + 1)
                        bullets(t).sy = 100 + PEEK(@y + 1)
                        CALL hires_set(bullets(t).sx, bullets(t).sy)
                    END IF
                END IF
            END IF
        NEXT
    END IF

    IF (game_time AND %00011111) = %00011111 THEN
        DIM speed AS INT
        speed = ABS(player.vx)
        IF ABS(player.vy) > speed THEN speed = ABS(player.vy)
        CALL hires_text(26, 0, 3, STR$(speed))
        CALL hires_text(26, 1, 4, STR$(PEEK(@player.x+2)))
        CALL hires_text(26, 2, 4, STR$(PEEK(@player.y+2)))
    END IF

    player.x = player.x + player.vx
    player.y = player.y + player.vy

    CALL sprite_shape(0, player.d)
    CALL sprite_shape(1, player.d + 16)

    'POKE 53280,0
    WAIT 53265, 128
    'POKE 53280,1
    game_time = game_time + 1
LOOP WHILE game_over=0

CALL sprite_off(0)
CALL sprite_off(1)
CALL hires_off()
END

SPRITE_DATA:
rem sprite N / singlecolor / color: $0c
DATA AS BYTE %00000000,%00010000,%00000000
DATA AS BYTE %00000000,%00010000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%01000100,%00000000
DATA AS BYTE %00000000,%01000100,%00000000
DATA AS BYTE %00000000,%10000010,%00000000
DATA AS BYTE %00000000,%10000010,%00000000
DATA AS BYTE %00000001,%00000001,%00000000
DATA AS BYTE %00000001,%00000001,%00000000
DATA AS BYTE %00000010,%00010000,%10000000
DATA AS BYTE %00000010,%00101000,%10000000
DATA AS BYTE %00000100,%01000100,%01000000
DATA AS BYTE %00000100,%10000010,%01000000
DATA AS BYTE %00001001,%00000001,%00100000
DATA AS BYTE %00001010,%00000000,%10100000
DATA AS BYTE %00001100,%00000000,%01100000
DATA AS BYTE %00001000,%00000000,%00100000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite NNE / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000001,%00000000
DATA AS BYTE %00000000,%00000011,%00000000
DATA AS BYTE %00000000,%00000101,%00000000
DATA AS BYTE %00000000,%00001001,%00000000
DATA AS BYTE %00000000,%00010001,%00000000
DATA AS BYTE %00000000,%00100001,%00000000
DATA AS BYTE %00000000,%01000001,%00000000
DATA AS BYTE %00000000,%10000001,%00000000
DATA AS BYTE %00000001,%00000001,%00000000
DATA AS BYTE %00000010,%00010001,%00000000
DATA AS BYTE %00000100,%01101001,%00000000
DATA AS BYTE %00001001,%10001001,%00000000
DATA AS BYTE %00010110,%00000101,%00000000
DATA AS BYTE %00111000,%00000101,%00000000
DATA AS BYTE %00000000,%00000011,%00000000
DATA AS BYTE %00000000,%00000011,%00000000
DATA AS BYTE %00000000,%00000001,%00000000
DATA AS BYTE %00000000,%00000001,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite NE / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00010000
DATA AS BYTE %00000000,%00000000,%11100000
DATA AS BYTE %00000000,%00000011,%00100000
DATA AS BYTE %00000000,%00001100,%00100000
DATA AS BYTE %00000000,%00110000,%01000000
DATA AS BYTE %00000000,%11000000,%01000000
DATA AS BYTE %00000011,%00000000,%10000000
DATA AS BYTE %00001100,%00000000,%10000000
DATA AS BYTE %00111111,%11110001,%00000000
DATA AS BYTE %00000000,%00010001,%00000000
DATA AS BYTE %00000000,%00010010,%00000000
DATA AS BYTE %00000000,%00010010,%00000000
DATA AS BYTE %00000000,%00010100,%00000000
DATA AS BYTE %00000000,%00010100,%00000000
DATA AS BYTE %00000000,%00011000,%00000000
DATA AS BYTE %00000000,%00011000,%00000000
DATA AS BYTE %00000000,%00010000,%00000000
DATA AS BYTE %00000000,%00010000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite ENE / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00011111,%11111111,%11111000
DATA AS BYTE %00000110,%00000000,%00010000
DATA AS BYTE %00000001,%10000000,%00100000
DATA AS BYTE %00000000,%01100000,%01000000
DATA AS BYTE %00000000,%00010000,%10000000
DATA AS BYTE %00000000,%00100001,%00000000
DATA AS BYTE %00000000,%00100010,%00000000
DATA AS BYTE %00000000,%01000100,%00000000
DATA AS BYTE %00000000,%01001000,%00000000
DATA AS BYTE %00000000,%10010000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000001,%01000000,%00000000
DATA AS BYTE %00000001,%10000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite E / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00001111,%00000000,%00000000
DATA AS BYTE %00000100,%11000000,%00000000
DATA AS BYTE %00000010,%00110000,%00000000
DATA AS BYTE %00000001,%00001100,%00000000
DATA AS BYTE %00000000,%10000011,%00000000
DATA AS BYTE %00000000,%01000000,%11000000
DATA AS BYTE %00000000,%00100000,%00110000
DATA AS BYTE %00000000,%00010000,%00001100
DATA AS BYTE %00000000,%00100000,%00110000
DATA AS BYTE %00000000,%01000000,%11000000
DATA AS BYTE %00000000,%10000011,%00000000
DATA AS BYTE %00000001,%00001100,%00000000
DATA AS BYTE %00000010,%00110000,%00000000
DATA AS BYTE %00000100,%11000000,%00000000
DATA AS BYTE %00001111,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite ESE / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000001,%10000000,%00000000
DATA AS BYTE %00000001,%01000000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10010000,%00000000
DATA AS BYTE %00000000,%01001000,%00000000
DATA AS BYTE %00000000,%01000100,%00000000
DATA AS BYTE %00000000,%00100010,%00000000
DATA AS BYTE %00000000,%00100001,%00000000
DATA AS BYTE %00000000,%00010000,%10000000
DATA AS BYTE %00000000,%01100000,%01000000
DATA AS BYTE %00000001,%10000000,%00100000
DATA AS BYTE %00000110,%00000000,%00010000
DATA AS BYTE %00011111,%11111111,%11111000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite SE / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00010000,%00000000
DATA AS BYTE %00000000,%00010000,%00000000
DATA AS BYTE %00000000,%00011000,%00000000
DATA AS BYTE %00000000,%00011000,%00000000
DATA AS BYTE %00000000,%00010100,%00000000
DATA AS BYTE %00000000,%00010100,%00000000
DATA AS BYTE %00000000,%00010010,%00000000
DATA AS BYTE %00000000,%00010010,%00000000
DATA AS BYTE %00000000,%00010001,%00000000
DATA AS BYTE %00111111,%11110001,%00000000
DATA AS BYTE %00001100,%00000000,%10000000
DATA AS BYTE %00000011,%00000000,%10000000
DATA AS BYTE %00000000,%11000000,%01000000
DATA AS BYTE %00000000,%00110000,%01000000
DATA AS BYTE %00000000,%00001100,%00100000
DATA AS BYTE %00000000,%00000011,%00100000
DATA AS BYTE %00000000,%00000000,%11100000
DATA AS BYTE %00000000,%00000000,%00010000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite SSE / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000001,%00000000
DATA AS BYTE %00000000,%00000001,%00000000
DATA AS BYTE %00000000,%00000011,%00000000
DATA AS BYTE %00000000,%00000011,%00000000
DATA AS BYTE %00111000,%00000101,%00000000
DATA AS BYTE %00010110,%00000101,%00000000
DATA AS BYTE %00001001,%10001001,%00000000
DATA AS BYTE %00000100,%01101001,%00000000
DATA AS BYTE %00000010,%00010001,%00000000
DATA AS BYTE %00000001,%00000001,%00000000
DATA AS BYTE %00000000,%10000001,%00000000
DATA AS BYTE %00000000,%01000001,%00000000
DATA AS BYTE %00000000,%00100001,%00000000
DATA AS BYTE %00000000,%00010001,%00000000
DATA AS BYTE %00000000,%00001001,%00000000
DATA AS BYTE %00000000,%00000101,%00000000
DATA AS BYTE %00000000,%00000011,%00000000
DATA AS BYTE %00000000,%00000001,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite S / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00001000,%00000000,%00100000
DATA AS BYTE %00001100,%00000000,%01100000
DATA AS BYTE %00001010,%00000000,%10100000
DATA AS BYTE %00001001,%00000001,%00100000
DATA AS BYTE %00000100,%10000010,%01000000
DATA AS BYTE %00000100,%01000100,%01000000
DATA AS BYTE %00000010,%00101000,%10000000
DATA AS BYTE %00000010,%00010000,%10000000
DATA AS BYTE %00000001,%00000001,%00000000
DATA AS BYTE %00000001,%00000001,%00000000
DATA AS BYTE %00000000,%10000010,%00000000
DATA AS BYTE %00000000,%10000010,%00000000
DATA AS BYTE %00000000,%01000100,%00000000
DATA AS BYTE %00000000,%01000100,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00010000,%00000000
DATA AS BYTE %00000000,%00010000,%00000000, $ff

REM sprite SSW / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%11000000,%00000000
DATA AS BYTE %00000000,%11000000,%00000000
DATA AS BYTE %00000000,%10100000,%00011100
DATA AS BYTE %00000000,%10100000,%01101000
DATA AS BYTE %00000000,%10010001,%10010000
DATA AS BYTE %00000000,%10010110,%00100000
DATA AS BYTE %00000000,%10001000,%01000000
DATA AS BYTE %00000000,%10000000,%10000000
DATA AS BYTE %00000000,%10000001,%00000000
DATA AS BYTE %00000000,%10000010,%00000000
DATA AS BYTE %00000000,%10000100,%00000000
DATA AS BYTE %00000000,%10001000,%00000000
DATA AS BYTE %00000000,%10010000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%11000000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite SW / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00010000,%00000000
DATA AS BYTE %00000000,%00010000,%00000000
DATA AS BYTE %00000000,%00110000,%00000000
DATA AS BYTE %00000000,%00110000,%00000000
DATA AS BYTE %00000000,%01010000,%00000000
DATA AS BYTE %00000000,%01010000,%00000000
DATA AS BYTE %00000000,%10010000,%00000000
DATA AS BYTE %00000000,%10010000,%00000000
DATA AS BYTE %00000001,%00010000,%00000000
DATA AS BYTE %00000001,%00011111,%11111000
DATA AS BYTE %00000010,%00000000,%01100000
DATA AS BYTE %00000010,%00000001,%10000000
DATA AS BYTE %00000100,%00000110,%00000000
DATA AS BYTE %00000100,%00011000,%00000000
DATA AS BYTE %00001000,%01100000,%00000000
DATA AS BYTE %00001001,%10000000,%00000000
DATA AS BYTE %00001110,%00000000,%00000000
DATA AS BYTE %00010000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite WSW / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000001,%10000000
DATA AS BYTE %00000000,%00000010,%10000000
DATA AS BYTE %00000000,%00000101,%00000000
DATA AS BYTE %00000000,%00001001,%00000000
DATA AS BYTE %00000000,%00010010,%00000000
DATA AS BYTE %00000000,%00100010,%00000000
DATA AS BYTE %00000000,%01000100,%00000000
DATA AS BYTE %00000000,%10000100,%00000000
DATA AS BYTE %00000001,%00001000,%00000000
DATA AS BYTE %00000010,%00000110,%00000000
DATA AS BYTE %00000100,%00000001,%10000000
DATA AS BYTE %00001000,%00000000,%01100000
DATA AS BYTE %00011111,%11111111,%11111000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite W / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000001,%11100000
DATA AS BYTE %00000000,%00000110,%01000000
DATA AS BYTE %00000000,%00011000,%10000000
DATA AS BYTE %00000000,%01100001,%00000000
DATA AS BYTE %00000001,%10000010,%00000000
DATA AS BYTE %00000110,%00000100,%00000000
DATA AS BYTE %00011000,%00001000,%00000000
DATA AS BYTE %01100000,%00010000,%00000000
DATA AS BYTE %00011000,%00001000,%00000000
DATA AS BYTE %00000110,%00000100,%00000000
DATA AS BYTE %00000001,%10000010,%00000000
DATA AS BYTE %00000000,%01100001,%00000000
DATA AS BYTE %00000000,%00011000,%10000000
DATA AS BYTE %00000000,%00000110,%01000000
DATA AS BYTE %00000000,%00000001,%11100000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite WNW / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00011111,%11111111,%11111000
DATA AS BYTE %00001000,%00000000,%01100000
DATA AS BYTE %00000100,%00000001,%10000000
DATA AS BYTE %00000010,%00000110,%00000000
DATA AS BYTE %00000001,%00001000,%00000000
DATA AS BYTE %00000000,%10000100,%00000000
DATA AS BYTE %00000000,%01000100,%00000000
DATA AS BYTE %00000000,%00100010,%00000000
DATA AS BYTE %00000000,%00010010,%00000000
DATA AS BYTE %00000000,%00001001,%00000000
DATA AS BYTE %00000000,%00000101,%00000000
DATA AS BYTE %00000000,%00000010,%10000000
DATA AS BYTE %00000000,%00000001,%10000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite NW / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00010000,%00000000,%00000000
DATA AS BYTE %00001110,%00000000,%00000000
DATA AS BYTE %00001001,%10000000,%00000000
DATA AS BYTE %00001000,%01100000,%00000000
DATA AS BYTE %00000100,%00011000,%00000000
DATA AS BYTE %00000100,%00000110,%00000000
DATA AS BYTE %00000010,%00000001,%10000000
DATA AS BYTE %00000010,%00000000,%01100000
DATA AS BYTE %00000001,%00011111,%11111000
DATA AS BYTE %00000001,%00010000,%00000000
DATA AS BYTE %00000000,%10010000,%00000000
DATA AS BYTE %00000000,%10010000,%00000000
DATA AS BYTE %00000000,%01010000,%00000000
DATA AS BYTE %00000000,%01010000,%00000000
DATA AS BYTE %00000000,%00110000,%00000000
DATA AS BYTE %00000000,%00110000,%00000000
DATA AS BYTE %00000000,%00010000,%00000000
DATA AS BYTE %00000000,%00010000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite NNW / singlecolor / color: $0c
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%11000000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10010000,%00000000
DATA AS BYTE %00000000,%10001000,%00000000
DATA AS BYTE %00000000,%10000100,%00000000
DATA AS BYTE %00000000,%10000010,%00000000
DATA AS BYTE %00000000,%10000001,%00000000
DATA AS BYTE %00000000,%10000000,%10000000
DATA AS BYTE %00000000,%10001000,%01000000
DATA AS BYTE %00000000,%10010110,%00100000
DATA AS BYTE %00000000,%10010001,%10010000
DATA AS BYTE %00000000,%10100000,%01101000
DATA AS BYTE %00000000,%10100000,%00011100
DATA AS BYTE %00000000,%11000000,%00000000
DATA AS BYTE %00000000,%11000000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite N / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00100000,%00000000
DATA AS BYTE %00000000,%00100000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000010,%10101010,%00000000
DATA AS BYTE %00000010,%10001010,%00000000
DATA AS BYTE %00000010,%10000010,%10000000
DATA AS BYTE %00000010,%00000010,%10000000
DATA AS BYTE %00001010,%00000000,%10000000
DATA AS BYTE %00001000,%00000000,%10000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite NNE / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000010,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00101010,%00000000
DATA AS BYTE %00000000,%00101010,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000010,%10101010,%00000000
DATA AS BYTE %00000010,%10001010,%00000000
DATA AS BYTE %00001010,%00001010,%00000000
DATA AS BYTE %00001000,%00000010,%00000000
DATA AS BYTE %00000000,%00000010,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite NE / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%10000000
DATA AS BYTE %00000000,%00000010,%10000000
DATA AS BYTE %00000000,%00001010,%10000000
DATA AS BYTE %00000000,%00101010,%10000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000010,%10101010,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00001000,%00000000
DATA AS BYTE %00000000,%00001000,%00000000
DATA AS BYTE %00000000,%00001000,%00000000
DATA AS BYTE %00000000,%00001000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite ENE / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000010,%10101010,%10100000
DATA AS BYTE %00000000,%10101010,%10000000
DATA AS BYTE %00000000,%00101010,%10000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00101010,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00100000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite E / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000010,%00000000,%00000000
DATA AS BYTE %00000010,%10000000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%00101010,%00000000
DATA AS BYTE %00000000,%00101010,%10000000
DATA AS BYTE %00000000,%00001010,%10100000
DATA AS BYTE %00000000,%00101010,%10000000
DATA AS BYTE %00000000,%00101010,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000010,%10000000,%00000000
DATA AS BYTE %00000010,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite ESE / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%00100000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00101010,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00101010,%10000000
DATA AS BYTE %00000000,%10101010,%10000000
DATA AS BYTE %00000010,%10101010,%10100000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite SE / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00001000,%00000000
DATA AS BYTE %00000000,%00001000,%00000000
DATA AS BYTE %00000000,%00001000,%00000000
DATA AS BYTE %00000000,%00001000,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000010,%10101010,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000000,%00101010,%10000000
DATA AS BYTE %00000000,%00001010,%10000000
DATA AS BYTE %00000000,%00000010,%10000000
DATA AS BYTE %00000000,%00000000,%10000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite SSE / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000010,%00000000
DATA AS BYTE %00001000,%00000010,%00000000
DATA AS BYTE %00001010,%00001010,%00000000
DATA AS BYTE %00000010,%10001010,%00000000
DATA AS BYTE %00000010,%10101010,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000000,%00101010,%00000000
DATA AS BYTE %00000000,%00101010,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00000010,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite S / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00001000,%00000000,%10000000
DATA AS BYTE %00001010,%00000000,%10000000
DATA AS BYTE %00000010,%00000010,%10000000
DATA AS BYTE %00000010,%10000010,%10000000
DATA AS BYTE %00000010,%10001010,%00000000
DATA AS BYTE %00000010,%10101010,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00100000,%00000000
DATA AS BYTE %00000000,%00100000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite SSW / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%10000000,%00100000
DATA AS BYTE %00000000,%10100000,%10100000
DATA AS BYTE %00000000,%10100010,%10000000
DATA AS BYTE %00000000,%10101010,%10000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite SW / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00100000,%00000000
DATA AS BYTE %00000000,%00100000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000010,%10101010,%10000000
DATA AS BYTE %00000010,%10101010,%00000000
DATA AS BYTE %00000010,%10101000,%00000000
DATA AS BYTE %00000010,%10100000,%00000000
DATA AS BYTE %00001010,%10000000,%00000000
DATA AS BYTE %00001010,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite WSW / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000010,%00000000
DATA AS BYTE %00000000,%00000010,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00001000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000010,%10101000,%00000000
DATA AS BYTE %00000010,%10101010,%00000000
DATA AS BYTE %00001010,%10101010,%10000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite W / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000010,%10000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00101010,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000010,%10101000,%00000000
DATA AS BYTE %00001010,%10100000,%00000000
DATA AS BYTE %00101010,%10100000,%00000000
DATA AS BYTE %00001010,%10100000,%00000000
DATA AS BYTE %00000010,%10101000,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%00101010,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00000010,%10000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite WNW / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00001010,%10101010,%10000000
DATA AS BYTE %00000010,%10101010,%00000000
DATA AS BYTE %00000010,%10101000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00101000,%00000000
DATA AS BYTE %00000000,%00001000,%00000000
DATA AS BYTE %00000000,%00001010,%00000000
DATA AS BYTE %00000000,%00000010,%00000000
DATA AS BYTE %00000000,%00000010,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite NW / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00001010,%00000000,%00000000
DATA AS BYTE %00001010,%10000000,%00000000
DATA AS BYTE %00000010,%10100000,%00000000
DATA AS BYTE %00000010,%10101000,%00000000
DATA AS BYTE %00000010,%10101010,%00000000
DATA AS BYTE %00000010,%10101010,%10000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%00100000,%00000000
DATA AS BYTE %00000000,%00100000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

REM sprite NNW / multicolor / color: $03
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10100000,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%10101000,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000000,%10101010,%00000000
DATA AS BYTE %00000000,%10101010,%10000000
DATA AS BYTE %00000000,%10100010,%10000000
DATA AS BYTE %00000000,%10100000,%10100000
DATA AS BYTE %00000000,%10000000,%00100000
DATA AS BYTE %00000000,%10000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000
DATA AS BYTE %00000000,%00000000,%00000000, $ff

CONST PORT_1 = $DC01
CONST PORT_2 = $DC00

_PORTS:
DATA AS WORD 0, $DC01, $DC00

CONST UP_MASK    = %11111110
CONST DOWN_MASK  = %11111101
CONST LEFT_MASK  = %11111011
CONST RIGHT_MASK = %11110111
CONST FIRE_MASK  = %11101111

DIM PORTS(3) AS WORD @_PORTS

FUNCTION joy_up AS BYTE (port AS BYTE) SHARED STATIC
    joy_up = (PEEK(PORTS(port)) OR UP_MASK) XOR $ff
END FUNCTION

FUNCTION joy_down AS BYTE (port AS BYTE) SHARED STATIC
    joy_down = (PEEK(PORTS(port)) OR DOWN_MASK) XOR $ff
END FUNCTION

FUNCTION joy_left AS BYTE (port AS BYTE) SHARED STATIC
    joy_left = (PEEK(PORTS(port)) OR LEFT_MASK) XOR $ff
END FUNCTION

FUNCTION joy_right AS BYTE (port AS BYTE) SHARED STATIC
    joy_right = (PEEK(PORTS(port)) OR RIGHT_MASK) XOR $ff
END FUNCTION

FUNCTION joy_fire AS BYTE (port AS BYTE) SHARED STATIC
    joy_fire = (PEEK(PORTS(port)) OR FIRE_MASK) XOR $ff
END FUNCTION

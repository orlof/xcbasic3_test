from math import sin, cos, pi

matrix = [None] * 256
for rotation in range(0, 32):
    for distance_index, distance in enumerate([0, 2, 4, 6, 8, 10, 12, 15]):
        angle = rotation * 2.0 * pi / 32.0
        x = round(distance * cos(angle) - 0 * sin(angle))
        y = -round(distance * sin(angle) + 0 * cos(angle))
        matrix[rotation << 3 | distance_index] = (x, y)

print("_RotX:")
for i in range(0, 256, 8):
    tmp = [i2[0] & 0xff for i2 in matrix[i:i+8]]
    tmp = map(str, tmp)
    print("DATA AS BYTE %s" % ",".join(tmp))

print("_RotY:")
for i in range(0, 256, 8):
    tmp = [i2[1] & 0xff for i2 in matrix[i:i+8]]
    tmp = map(str, tmp)
    print("DATA AS BYTE %s" % ",".join(tmp))

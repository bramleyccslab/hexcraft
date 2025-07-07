import os

BOARD_SIZE = 9

def print_board(movable_shapes, locked_shapes):
    os.system('cls' if os.name == 'nt' else 'clear')
    half = BOARD_SIZE // 2
    start = BOARD_SIZE - half
    increasing = list(range(start, BOARD_SIZE + 1))
    decreasing = list(range(BOARD_SIZE - 1, start - 1, -1))
    row_lengths = increasing + decreasing

    start = 2 + half
    decreasing = list(range(start, 1, -1))
    increasing = list(range(3, start + 1))
    leading_spaces = decreasing + increasing

    for y in range(BOARD_SIZE):
        row_str = " " * leading_spaces[y]
        
        for x in range(row_lengths[y]):
            occupied = any((y, x) in shape for shape in [movable_shapes[0] + locked_shapes[0]])
            row_str += '■ ' if occupied else '· '
        
        print(row_str)

    print("\na=add new piece at center, d=delete new piece at center, l=lock all pieces, r=rotate all pieces clockwise, f=flip, w=shift west, q=quit")

def create_center(movable_shapes):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    movable_shapes[0].append((cx, cy))

    return movable_shapes

def delete_center(movable_shapes):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    for shape in movable_shapes:
        while (cx, cy) in shape:
            shape.remove((cx, cy))
    return movable_shapes

def lock(movable_shapes, locked_shapes):
    # once we lock all shapes, they cannot be removed   
    locked_shapes = [movable_shapes[0] + locked_shapes[0]]
    movable_shapes = [[]]
    return movable_shapes, locked_shapes

def move_west(movable_shapes):
    for shape in movable_shapes:
        for i in range(len(shape)):
            x, y = shape[i]
            shape[i] = (x, y - 1)
    return movable_shapes

def offset_to_axial(x, y):
    if x <= BOARD_SIZE // 2:
        q = y - x
    else:
        q = y - BOARD_SIZE // 2
    r = x - BOARD_SIZE // 2
    return q, r

def axial_to_offset(q, r):
    x = r + BOARD_SIZE // 2
    if x <= BOARD_SIZE // 2:
        y = q + r + BOARD_SIZE // 2
    else:
        y = q + BOARD_SIZE // 2
    return x, y

def axial_to_cube(q, r):
    x = q
    z = r
    y = -x - z
    return (x, y, z)

def cube_to_axial(x, y, z):
    q = x
    r = z
    return (q, r)

def rotate60(a, b):
    q, r = offset_to_axial(a, b)
    x, y, z = axial_to_cube(q, r)
    x, y, z = -z, -x, -y

    return axial_to_offset(*cube_to_axial(x, y, z))

def flip_nw_to_se(a, b):
    # the axis is q = 0 in axial coordinates
    q, r = offset_to_axial(a, b)

    return axial_to_offset(-q, q + r)

def rotate(movable_shapes):
    for shape in movable_shapes:
        for i in range(len(shape)):
            x, y = shape[i]
            shape[i] = rotate60(x, y)
    return movable_shapes

def flip(movable_shapes):
    for shape in movable_shapes:
        for i in range(len(shape)):
            x, y = shape[i]
            shape[i] = flip_nw_to_se(x, y)
    return movable_shapes

def main():
    movable_shapes = [[]]
    locked_shapes = [[]] 
    print_board(movable_shapes, locked_shapes)
    while True:
        command = input("Please enter your command: ").strip().lower()
        if command == 'q':
            break
        elif command == 'a':
            movable_shapes = create_center(movable_shapes)
        elif command == 'd':
            movable_shapes = delete_center(movable_shapes)
        elif command == 'f':
            movable_shapes = flip(movable_shapes)
        elif command == 'w':
            movable_shapes = move_west(movable_shapes)
        elif command == 'l':
            movable_shapes, locked_shapes = lock(movable_shapes, locked_shapes)
        elif command == 'r':
            movable_shapes = rotate(movable_shapes)
        print_board(movable_shapes, locked_shapes)

if __name__ == "__main__":
    main()
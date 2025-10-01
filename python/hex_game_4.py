import os

BOARD_SIZE = 9

GOAL_SHAPE = [(BOARD_SIZE // 2, BOARD_SIZE // 2), (BOARD_SIZE // 2, BOARD_SIZE // 2 + 1)]

def check_goal(shapes, goal_shape):
    return set(shapes[0]) == set(goal_shape)

def generate_board_string(shapes, label="Board"):
    half = BOARD_SIZE // 2
    start = BOARD_SIZE - half
    increasing = list(range(start, BOARD_SIZE + 1))
    decreasing = list(range(BOARD_SIZE - 1, start - 1, -1))
    row_lengths = increasing + decreasing

    start = 2 + half
    decreasing = list(range(start, 1, -1))
    increasing = list(range(3, start + 1))
    leading_spaces = decreasing + increasing

    board_lines = [f"{label}:"]
    shape_set = set(shapes[0])

    for y in range(BOARD_SIZE):
        row_str = " " * leading_spaces[y]
        for x in range(row_lengths[y]):
            row_str += '■ ' if (y, x) in shape_set else '· '
        board_lines.append(row_str)
    return "\n".join(board_lines)

def print_two_boards(goal_shape, current_shapes):
    os.system('cls' if os.name == 'nt' else 'clear')
    goal_str = generate_board_string([goal_shape], "Target Shape")
    current_str = generate_board_string(current_shapes, "Your Shape")
    print(goal_str + "\n\n" + current_str)
    print("\na=add center, d=delete center, z=corner, x=bar, w=west, e=northeast, s=southeast, f=flip, r=reflect, space=rotate, q=quit")

def create_center(shapes):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    shapes[0].append((cx, cy))

    return shapes

def add_bar(shapes):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    bar = [(cx - 1, cy - 1), (cx, cy), (cx + 1, cy)]
    shapes[0].extend(bar)
    return shapes

def add_corner(shapes):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    corner = [(cx + 1, cy - 1), (cx, cy), (cx, cy + 1)]
    shapes[0].extend(corner)
    return shapes

def delete_center(shapes):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    for shape in shapes:
        while (cx, cy) in shape:
            shape.remove((cx, cy))
    return shapes

def move_west(shapes):
    for shape in shapes:
        for i in range(len(shape)):
            x, y = shape[i]
            shape[i] = (x, y - 1)
    return shapes

def move_northeast(shapes):
    for shape in shapes:
        for i in range(len(shape)):
            x, y = shape[i]
            q, r = offset_to_axial(x, y)
            q += 1
            r -= 1
            shape[i] = axial_to_offset(q, r)
    return shapes

def move_southeast(shapes):
    for shape in shapes:
        for i in range(len(shape)):
            x, y = shape[i]
            q, r = offset_to_axial(x, y)
            r += 1
            shape[i] = axial_to_offset(q, r)
    return shapes

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
    q, r = offset_to_axial(a, b)
    return axial_to_offset(-q, q + r)

def flip_ne_to_sw(a, b):
    q, r = offset_to_axial(a, b)
    return axial_to_offset(q, -q - r)

def flip_w_to_e(a, b):
    q, r = offset_to_axial(a, b)
    # s = -q - r
    return axial_to_offset(-q - r, r)

def rotate(shapes):
    for shape in shapes:
        for i in range(len(shape)):
            x, y = shape[i]
            shape[i] = rotate60(x, y)
    return shapes

def flip(shapes):
    for shape in shapes:
        for i in range(len(shape)):
            x, y = shape[i]
            shape[i] = flip_w_to_e(x, y)
    return shapes

def reflect(shapes):
    reflected_shapes = []
    for shape in shapes:
        original = shape[:]
        flipped = [flip_w_to_e(x, y) for (x, y) in shape]
        combined = list(set(original + flipped))
        reflected_shapes.append(combined)
    return reflected_shapes

def main():
    shapes = [[]]
    print_two_boards(GOAL_SHAPE, shapes)
    while True:
        command = input("Please enter your command: ").lower()
        if command == 'q':
            break
        elif command == 'a':
            shapes = create_center(shapes)
        elif command == 'd':
            shapes = delete_center(shapes)
        elif command == 'z':
            shapes = add_corner(shapes)
        elif command == 'x':
            shapes = add_bar(shapes)
        elif command == 'w':
            shapes = move_west(shapes)
        elif command == 'e':
            shapes = move_northeast(shapes)
        elif command == 's':
            shapes = move_southeast(shapes)
        elif command == 'f':
            shapes = flip(shapes)
        elif command == 'r':
            shapes = reflect(shapes)
        elif command == ' ':
            shapes = rotate(shapes)

        print_two_boards(GOAL_SHAPE, shapes)

        if check_goal(shapes, GOAL_SHAPE):
            print("Goal Completed!")
            break

if __name__ == "__main__":
    main()
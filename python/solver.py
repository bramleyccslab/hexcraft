import itertools

BOARD_SIZE = 9

def check_goal(shape, goal_shape):
    return set(shape) == set(goal_shape)

def get_shape_from_sequence(sequence):
    shape = []
    for cmd in sequence:
        func = ACTIONS[cmd]
        shape = func(shape)
    return shape

def create_center(shape):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    shape.append((cx, cy))

    return shape

def add_bar(shape):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    bar = [(cx - 1, cy - 1), (cx, cy), (cx + 1, cy)]
    shape.extend(bar)
    return shape

def add_corner(shape):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    corner = [(cx + 1, cy - 1), (cx, cy), (cx, cy + 1)]
    shape.extend(corner)
    return shape

def delete_center(shape):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    while (cx, cy) in shape:
        shape.remove((cx, cy))
    return shape

def move_west(shape):
    for i in range(len(shape)):
        x, y = shape[i]
        shape[i] = (x, y - 1)
    return shape

def move_northeast(shape):
    for i in range(len(shape)):
        x, y = shape[i]
        q, r = offset_to_axial(x, y)
        q += 1
        r -= 1
        shape[i] = axial_to_offset(q, r)
    return shape

def move_southeast(shape):
    for i in range(len(shape)):
        x, y = shape[i]
        q, r = offset_to_axial(x, y)
        r += 1
        shape[i] = axial_to_offset(q, r)
    return shape

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

def rotate(shape):
    for i in range(len(shape)):
        x, y = shape[i]
        shape[i] = rotate60(x, y)
    return shape

def flip(shape):
    for i in range(len(shape)):
        x, y = shape[i]
        shape[i] = flip_w_to_e(x, y)
    return shape

def reflect(shape):
    original = shape[:]
    flipped = [flip_w_to_e(x, y) for (x, y) in shape]
    combined = list(set(original + flipped))
    return combined

ACTIONS = {
    'a': create_center,
    'd': delete_center,
    'z': add_corner,
    'x': add_bar,
    'w': move_west,
    'e': move_northeast,
    's': move_southeast,  
    'f': flip,
    'r': reflect,
    ' ': rotate,

    'wwr r r': lambda shape: reflect(rotate(reflect(rotate(reflect(move_west(move_west(shape))))))),
    'zxdr': lambda shape: reflect(delete_center(add_bar(add_corner(shape)))),
}

def goal_solver(goal_shape, max_depth=7):
    action_keys = list(ACTIONS.keys())
    solutions = []

    for length in range(1, max_depth + 1):
        for cmd_seq in itertools.product(action_keys, repeat=length):
            shapes = []
            for cmd in cmd_seq:
                func = ACTIONS[cmd]
                shapes = func(shapes)
            if check_goal(shapes, goal_shape):
                solutions.append(list(cmd_seq))

    return solutions

def convert_string(s):
    # convert action sequences to usable input to our program
    result = []
    for char in s:
        if char == 'K':
            result.append(' ')
        else:
            result.append(char.lower())
    return result

def main():
    GOAL_SHAPE = get_shape_from_sequence(convert_string('AWAWAWWRKRKR'))

    print("Searching for solutions...")
    sols = goal_solver(GOAL_SHAPE, max_depth=5)
    for i, sol in enumerate(sols):
        print(f"Solution {i+1}: {' -> '.join(sol)}")
    print(f"Total solutions found: {len(sols)}")

if __name__ == "__main__":
    main()
import itertools

BOARD_SIZE = 9

GOAL_SHAPE = [(BOARD_SIZE // 2, BOARD_SIZE // 2), (BOARD_SIZE // 2, BOARD_SIZE // 2 + 1)]

def check_goal(shapes, goal_shape):
    return set(shapes[0]) == set(goal_shape)

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

def create_center(shapes):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    shapes[0].append((cx, cy))
    return shapes

def add_bar(shapes):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    bar = [(cx, cy - 1), (cx, cy), (cx, cy + 1)]
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
            shape[i] = flip_nw_to_se(x, y)
    return shapes

def reflect(shapes):
    reflected_shapes = []
    for shape in shapes:
        original = shape[:]
        flipped = [flip_nw_to_se(x, y) for (x, y) in shape]
        combined = list(set(original + flipped))
        reflected_shapes.append(combined)
    return reflected_shapes

ACTIONS = {
    'a': create_center,
    'b': add_bar,
    'c': add_corner,
    'd': delete_center,
    'f': flip,
    'r': rotate,
    're': reflect,
    'w': move_west,
}

def goal_solver_brute_force(goal_shape, max_depth=7):
    action_keys = list(ACTIONS.keys())
    solutions = []

    for length in range(1, max_depth + 1):
        for cmd_seq in itertools.product(action_keys, repeat=length):
            shapes = [[]]
            for cmd in cmd_seq:
                func = ACTIONS[cmd]
                shapes = func(shapes)
            if check_goal(shapes, goal_shape):
                solutions.append(list(cmd_seq))

    return solutions

def main():
    print("Searching for solutions...")
    sols = goal_solver_brute_force(GOAL_SHAPE, max_depth=7)
    for i, sol in enumerate(sols):
        print(f"Solution {i+1}: {' -> '.join(sol)}")
    print(f"Total solutions found: {len(sols)}")

if __name__ == "__main__":
    main()
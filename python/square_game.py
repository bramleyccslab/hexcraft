import os

BOARD_SIZE = 11
board = [[0 for _ in range(BOARD_SIZE)] for _ in range(BOARD_SIZE)]

def print_board(current_shapes):
    os.system('cls' if os.name == 'nt' else 'clear')
    for y in range(BOARD_SIZE):
        row = ""
        for x in range(BOARD_SIZE):
            occupied = any((x, y) in shape for shape in current_shapes)
            row += '■' if occupied else '·'
        print(row)
    print("\nl=new l shape, f=flip, r=clockwise rotate, wasd=move, q=quit")

def create_L():
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    return [
        (cx, cy - 1),
        (cx, cy),
        (cx, cy + 1),
        (cx + 1, cy + 1)
    ]

def move_all(shapes, dx, dy):
    new_shapes = [
        [(x + dx, y + dy) for x, y in shape]
        for shape in shapes
    ]
    if all(0 <= x < BOARD_SIZE and 0 <= y < BOARD_SIZE for shape in new_shapes for x, y in shape):
        return new_shapes
    return shapes

def rotate_all(shapes):
    cx, cy = BOARD_SIZE // 2, BOARD_SIZE // 2
    new_shapes = []
    for shape in shapes:
        rotated = []
        for x, y in shape:
            dx, dy = x - cx, y - cy
            rx, ry = cx - dy, cy + dx
            rotated.append((rx, ry))
        new_shapes.append(rotated)
    if all(0 <= x < BOARD_SIZE and 0 <= y < BOARD_SIZE for shape in new_shapes for x, y in shape):
        return new_shapes
    return shapes

def flip_all(shapes):
    cx = BOARD_SIZE // 2
    new_shapes = []
    for shape in shapes:
        flipped = []
        for x, y in shape:
            fx = 2 * cx - x
            flipped.append((fx, y))
        new_shapes.append(flipped)
    if all(0 <= x < BOARD_SIZE and 0 <= y < BOARD_SIZE for shape in new_shapes for x, y in shape):
        return new_shapes
    return shapes

def main():
    current_shapes = []
    print_board(current_shapes)
    while True:
        command = input("Please enter your command: ").strip().lower()
        if command == 'q':
            break
        elif command == 'l':
            current_shapes.append(create_L())
        elif command == 'f':
            current_shapes = flip_all(current_shapes)
        elif command == 'r':
            current_shapes = rotate_all(current_shapes)
        elif command == 'w':
            current_shapes = move_all(current_shapes, 0, -1)
        elif command == 's':
            current_shapes = move_all(current_shapes, 0, 1)
        elif command == 'a':
            current_shapes = move_all(current_shapes, -1, 0)
        elif command == 'd':
            current_shapes = move_all(current_shapes, 1, 0)
        print_board(current_shapes)

if __name__ == "__main__":
    main()
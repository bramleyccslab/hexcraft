import random
import json
from tqdm import tqdm
import ast

BOARD_SIZE = 9

# First we declare some IO functions about Q
# We initialize the Q value of every movement to be 0 
def get_Q(Q, state, action):
    return Q.get((tuple(state), action), 0.0)

def load_Q(filename):
    with open(filename, 'r') as f:
        Q_serializable = json.load(f)
    Q = {ast.literal_eval(k): v for k, v in Q_serializable.items()}
    return Q

def save_Q(Q, filename):
    Q_serializable = {str(k): v for k, v in Q.items()}
    with open(filename, 'w') as f:
        json.dump(Q_serializable, f)

# Then we declare some functions about choosing actions during training and inference
def choose_action(Q, state, actions, epsilon):
    # epsilon-greedy (training phase)
    if random.random() < epsilon:
        return random.choice(actions)
    else:
        q_values = [(get_Q(Q, state, a), a) for a in actions]
        max_q = max(q_values, key=lambda x: x[0])[0]
        best_actions = [a for (q, a) in q_values if q == max_q]
        return random.choice(best_actions)

def get_best_action(Q, state, actions):
    # choose the action that corresponds to the biggest Q (inference phase)
    q_values = [(get_Q(Q, state, a), a) for a in actions]
    max_q = max(q_values, key=lambda x: x[0])[0]
    best_actions = [a for (q, a) in q_values if abs(q - max_q) < 1e-9]
    return random.choice(best_actions)

# Here are the functions related to the hexcraft game
def check_goal(shapes, goal_shape):
    return set(shapes) == set(goal_shape)

def convert_string(s):
    # convert action sequences to usable input to our program
    result = []
    for char in s:
        if char == 'K':
            result.append(' ')
        else:
            result.append(char.lower())
    return result

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
}

def get_shape_from_sequence(sequence):
    shape = []
    for cmd in sequence:
        func = ACTIONS[cmd]
        shape = func(shape)
    return shape

def hex_distance(a1, b1, a2, b2):
    q1, r1 = offset_to_axial(a1, b1)
    q2, r2 = offset_to_axial(a2, b2)
    x1, y1, z1 = axial_to_cube(q1, r1)
    x2, y2, z2 = axial_to_cube(q2, r2)

    return (abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)) / 2

def chamfer_distance_hex(shape1, shape2):
    empty1 = (len(shape1) == 0)
    empty2 = (len(shape2) == 0)

    if empty1 and empty2:
        return 0.0

    elif empty1 or empty2:
        return 10.0
    
    dist_a_to_b = []
    for a in shape1:
        min_d = min(hex_distance(*a, *b) for b in shape2)
        dist_a_to_b.append(min_d)

    dist_b_to_a = []
    for b in shape2:
        min_d = min(hex_distance(*a, *b) for a in shape1)
        dist_b_to_a.append(min_d)

    return (sum(dist_a_to_b) / len(shape1)) + (sum(dist_b_to_a) / len(shape2))

# def get_reward(state, goal_state):
#     return 1.0 if check_goal(state, goal_state) else 0.0

def get_reward(prev_state, curr_state, goal_state):
    return chamfer_distance_hex(prev_state, goal_state) - chamfer_distance_hex(curr_state, goal_state)

# A simple generation from the ACTIONS, not the probabilistic generation
def generate_simple(char_list, length, number):
    return [random.choices(char_list, k=length) for _ in range(number)]

# The main Q-learning function
def q_learning(filename, goal_states, alpha=0.1, gamma=0.9, epsilon=0.2, episodes=1000, max_steps=10):
    Q = {}
    actions = list(ACTIONS.keys())

    assert len(goal_states) == episodes

    for ep in tqdm(range(episodes)):
        state = []
        goal_state = goal_states[ep]
        for _ in range(max_steps):
            action = choose_action(Q, state, actions, epsilon)

            func = ACTIONS[action]
            next_state = func(state)

            reward = get_reward(state, next_state, goal_state)

            max_next_q = max([get_Q(Q, next_state, a) for a in actions], default=0.0)

            old_q = get_Q(Q, state, action)
            new_q = old_q + alpha * (reward + gamma * max_next_q - old_q)
            Q[(tuple(state), action)] = new_q

            state = next_state

            if check_goal(state, goal_state):
                break

    save_Q(Q, filename)

# The function for inference after having learned a Q dictionary
# no MCMC-like caches
def q_inference(filename, goal_state, max_steps=10):
    Q = load_Q(filename)
    state = []
    actions_list = []
    actions = list(ACTIONS.keys())

    for _ in range(max_steps):
        action = get_best_action(Q, state, actions)

        func = ACTIONS[action]
        state = func(state)
        actions_list.append(action)

        if check_goal(state, goal_state):
            break

    return actions_list

def main():
    # We use <episodes> training shapes with a length of <train_sequence_length> to train the model 
    episodes = 100000
    train_sequence_length = 5
    train_actions = generate_simple(list(ACTIONS.keys()), train_sequence_length, episodes)
    train_goal_shapes = [get_shape_from_sequence(actions) for actions in train_actions]
    q_learning("q.json", train_goal_shapes, episodes=episodes)

    inference_goal_shape = get_shape_from_sequence(convert_string('ZXDR'))
    print(q_inference("q.json", inference_goal_shape))
    

if __name__ == "__main__":
    main()
import random
import json
from tqdm import tqdm

BOARD_SIZE = 9

def to_hashable(obj):
    if isinstance(obj, list):
        return tuple(to_hashable(item) for item in obj)
    return obj

# First we declare some IO functions about Q
# We initialize the Q value of every movement to be 0 
def get_Q(Q, state, action):
    return Q.get((to_hashable(state), action), 0.0)

def load_Q(filename):
    with open(filename, 'r') as f:
        Q_serializable = json.load(f)
    Q = {eval(k): v for k, v in Q_serializable.items()}
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
    return set(shapes[0]) == set(goal_shape)

def convert_string(s):
    # convert action sequences to usable input to our program
    result = []
    for char in s:
        if char == 'K':
            result.append(' ')
        else:
            result.append(char.lower())
    return result

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
    shapes = [[]]
    for cmd in sequence:
        func = ACTIONS[cmd]
        shapes = func(shapes)
    return shapes[0]

# We can change the reward function as we need
def get_reward(state, goal_state):
    return 1.0 if check_goal(state, goal_state) else 0.0

# A simple generation from the ACTIONS, not the probabilistic generation
def generate_simple(char_list, length, number):
    return [random.choices(char_list, k=length) for _ in range(number)]

# The main Q-learning function
def q_learning(filename, goal_states, alpha=0.1, gamma=0.9, epsilon=0.2, episodes=1000, max_steps=10):
    Q = {}
    actions = list(ACTIONS.keys())

    assert len(goal_states) == episodes

    for ep in tqdm(range(episodes)):
        state = [[]]
        goal_state = goal_states[ep]
        for _ in range(max_steps):
            action = choose_action(Q, state, actions, epsilon)

            func = ACTIONS[action]
            next_state = func(state)

            reward = get_reward(state, goal_state)

            max_next_q = max([get_Q(Q, next_state, a) for a in actions], default=0.0)

            old_q = get_Q(Q, state, action)
            new_q = old_q + alpha * (reward + gamma * max_next_q - old_q)
            Q[(to_hashable(state), action)] = new_q

            state = next_state

            if check_goal(state, goal_state):
                break

    save_Q(Q, filename)

# The function for inference after having learned a Q dictionary
# no MCMC-like caches
def q_inference(filename, goal_state, max_steps=10):
    Q = load_Q(filename)
    state = [[]]
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

    inference_goal_shape = get_shape_from_sequence(convert_string('ZXDRWWRKRKR'))
    print(q_inference("q.json", inference_goal_shape))
    

if __name__ == "__main__":
    main()
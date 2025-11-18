import random
import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
from collections import deque
from tqdm import tqdm
import time
from torch.utils.tensorboard import SummaryWriter

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

BOARD_SIZE = 9

class DQN(nn.Module):
    def __init__(self, state_size, action_size, hidden_size=256):
        super(DQN, self).__init__()
        self.fc1 = nn.Linear(state_size, hidden_size)
        self.fc2 = nn.Linear(hidden_size, hidden_size)
        self.fc3 = nn.Linear(hidden_size, hidden_size)
        self.fc4 = nn.Linear(hidden_size, action_size)
        self.relu = nn.ReLU()
        
    def forward(self, x):
        x = self.relu(self.fc1(x))
        x = self.relu(self.fc2(x))
        x = self.relu(self.fc3(x))
        return self.fc4(x)

# turn the state into something understandable by the NN
def state_to_tensor(state):
    board = np.zeros((BOARD_SIZE, BOARD_SIZE), dtype=np.float32)
    for x, y in state:
        if 0 <= x < BOARD_SIZE and 0 <= y < BOARD_SIZE:
            board[x, y] = 1.0
    return torch.FloatTensor(board.flatten()).to(device)

ACTION_LIST = ['a', 'd', 'z', 'x', 'w', 'e', 's', 'f', 'r', ' ']
ACTION_SIZE = len(ACTION_LIST)
ACTION_TO_IDX = {action: idx for idx, action in enumerate(ACTION_LIST)}

class ReplayBuffer:
    def __init__(self, capacity=10000):
        self.buffer = deque(maxlen=capacity)
    
    def push(self, state, action, reward, next_state, done):
        self.buffer.append((state, action, reward, next_state, done))
    
    def sample(self, batch_size):
        batch = random.sample(self.buffer, batch_size)
        state, action, reward, next_state, done = map(np.stack, zip(*batch))
        return state, action, reward, next_state, done
    
    def __len__(self):
        return len(self.buffer)

def linear_schedule(start_e: float, end_e: float, duration: int, t: int):
    slope = (end_e - start_e) / duration
    return max(slope * t + start_e, end_e)

def check_goal(shapes, goal_shape):
    return set(shapes) == set(goal_shape)

def convert_string(s):
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

def get_reward(prev_state, curr_state, goal_state):
    return chamfer_distance_hex(prev_state, goal_state) - chamfer_distance_hex(curr_state, goal_state)

def generate_simple(char_list, length, number):
    return [random.choices(char_list, k=length) for _ in range(number)]

def dqn_learning(filename, goal_states, episodes, alpha=0.001, gamma=0.99, epsilon_start=1.0, epsilon_end=0.05, 
                 exploration_fraction=0.5, max_steps=10, batch_size=64, 
                 target_update_freq=100, learning_starts=1000, train_frequency=10):
    state_size = BOARD_SIZE * BOARD_SIZE
    action_size = len(ACTION_LIST)
    
    dqn = DQN(state_size, action_size).to(device)
    target_dqn = DQN(state_size, action_size).to(device)
    target_dqn.load_state_dict(dqn.state_dict())
    
    optimizer = optim.Adam(dqn.parameters(), lr=alpha)
    criterion = nn.MSELoss()
    
    replay_buffer = ReplayBuffer()
    
    actions = list(ACTIONS.keys())
    
    assert len(goal_states) == episodes

    writer = SummaryWriter(f"runs/hexcraft_dqn_{int(time.time())}")
    
    writer.add_text(
        "hyperparameters",
        "|param|value|\n|-|-|\n"
        f"|learning_rate|{alpha}|\n"
        f"|gamma|{gamma}|\n"
        f"|epsilon_start|{epsilon_start}|\n"
        f"|epsilon_end|{epsilon_end}|\n"
        f"|exploration_fraction|{exploration_fraction}|\n"
        f"|episodes|{episodes}|\n"
        f"|batch_size|{batch_size}|",
    )

    start_time = time.time()
    total_steps = episodes * max_steps
    exploration_duration = int(exploration_fraction * total_steps)

    for ep in tqdm(range(episodes)):
        state = []
        goal_state = goal_states[ep]
        
        current_state_tensor = state_to_tensor(state)
        
        for step in range(max_steps):
            global_step = ep * max_steps + step
            epsilon = linear_schedule(epsilon_start, epsilon_end, exploration_duration, global_step)
            
            if random.random() < epsilon:
                action = random.choice(actions)
            else:
                with torch.no_grad():
                    q_values = dqn(current_state_tensor.unsqueeze(0))
                    action_idx = q_values.max(1)[1].item()
                    action = ACTION_LIST[action_idx]
            
            func = ACTIONS[action]
            next_state = func(state.copy())
            
            reward = get_reward(state, next_state, goal_state)
            
            # extra reward if the model gets to the goal
            done = check_goal(next_state, goal_state)
            if done:
                reward += 10.0 
            
            next_state_tensor = state_to_tensor(next_state)
            
            action_idx = ACTION_TO_IDX[action]
            replay_buffer.push(current_state_tensor.cpu().numpy(), action_idx, 
                              reward, next_state_tensor.cpu().numpy(), done)
            
            state = next_state
            current_state_tensor = next_state_tensor
            
            if len(replay_buffer) > learning_starts and global_step % train_frequency == 0:
                batch_state, batch_action, batch_reward, batch_next_state, batch_done = replay_buffer.sample(batch_size)
                
                batch_state = torch.FloatTensor(batch_state).to(device)
                batch_action = torch.LongTensor(batch_action).to(device)
                batch_reward = torch.FloatTensor(batch_reward).to(device)
                batch_next_state = torch.FloatTensor(batch_next_state).to(device)
                batch_done = torch.BoolTensor(batch_done).to(device)
                
                current_q_values = dqn(batch_state).gather(1, batch_action.unsqueeze(1))
                
                with torch.no_grad():
                    next_q_values = target_dqn(batch_next_state).max(1)[0]
                    target_q_values = batch_reward + (gamma * next_q_values * ~batch_done)
                
                loss = criterion(current_q_values.squeeze(), target_q_values)
                
                optimizer.zero_grad()
                loss.backward()
                optimizer.step()
                
                if global_step % 100 == 0:
                    writer.add_scalar("losses/td_loss", loss, global_step)
                    writer.add_scalar("losses/q_values", current_q_values.mean().item(), global_step)
                    writer.add_scalar("charts/epsilon", epsilon, global_step)
                    writer.add_scalar("charts/SPS", int(global_step / (time.time() - start_time)), global_step)
            
            if global_step % target_update_freq == 0:
                target_dqn.load_state_dict(dqn.state_dict())
            
            if done:
                writer.add_scalar("charts/episodic_return", reward, ep)
                writer.add_scalar("charts/episodic_length", step + 1, ep)
                break
    
    torch.save(dqn.state_dict(), filename)
    
    writer.add_scalar("charts/total_training_time", time.time() - start_time, episodes)
    writer.close()

def dqn_inference(filename, goal_state, max_steps=10):
    state_size = BOARD_SIZE * BOARD_SIZE
    action_size = len(ACTION_LIST)
    
    dqn = DQN(state_size, action_size).to(device)
    dqn.load_state_dict(torch.load(filename, map_location=device))
    dqn.eval()
    
    state = []
    actions_list = []
    
    for _ in range(max_steps):
        with torch.no_grad():
            state_tensor = state_to_tensor(state)
            q_values = dqn(state_tensor.unsqueeze(0))
            action_idx = q_values.max(1)[1].item()
            action = ACTION_LIST[action_idx]
        
        func = ACTIONS[action]
        state = func(state)
        actions_list.append(action)
        
        if check_goal(state, goal_state):
            break
    
    return actions_list

def main():
    episodes = 5000
    train_sequence_length = 5
    train_actions = generate_simple(list(ACTIONS.keys()), train_sequence_length, episodes)
    train_goal_shapes = [get_shape_from_sequence(actions) for actions in train_actions]
    
    dqn_learning("dqn_model.pth", train_goal_shapes, episodes=episodes)

    inference_goal_shape = get_shape_from_sequence(convert_string('ZXDR'))
    print(dqn_inference("dqn_model.pth", inference_goal_shape))

if __name__ == "__main__":
    main()

# 过多空的训练数据

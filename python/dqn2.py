import os
os.environ["KMP_DUPLICATE_LIB_OK"] = "TRUE"

import random
import numpy as np
import torch
import json
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from collections import deque
from tqdm import tqdm
import time
import matplotlib.pyplot as plt
from torch.utils.tensorboard import SummaryWriter

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

BOARD_SIZE = 9

class DQN(nn.Module):
    def __init__(self, action_size):
        super().__init__()
        self.conv1 = nn.Conv2d(2, 32, kernel_size=3, padding=1)
        self.conv2 = nn.Conv2d(32, 64, kernel_size=3, padding=1)
        self.fc = nn.Sequential(
            nn.Linear(64 * BOARD_SIZE * BOARD_SIZE, 256),
            nn.ReLU(),
            nn.Linear(256, action_size)
        )

    def forward(self, x):
        x = F.relu(self.conv1(x))
        x = F.relu(self.conv2(x))
        x = x.view(x.size(0), -1)
        return self.fc(x)

def shape_to_board(shape):
    board = np.zeros((BOARD_SIZE, BOARD_SIZE), dtype=np.float32)
    for x, y in shape:
        if 0 <= x < BOARD_SIZE and 0 <= y < BOARD_SIZE:
            board[x, y] = 1.0
    return board

def state_goal_to_tensor(state, goal):
    s = shape_to_board(state)
    g = shape_to_board(goal)
    stacked = np.stack([s, g], axis=0)  # (2,9,9)
    return torch.FloatTensor(stacked).to(device)

ACTION_LIST = ['a', 'd', 'z', 'x', 'w', 'e', 's', 'f', 'r', ' ']
ACTION_SIZE = len(ACTION_LIST)
ACTION_TO_IDX = {a: i for i, a in enumerate(ACTION_LIST)}

class PrioritizedReplayBuffer:
    def __init__(self, capacity=10000, alpha=0.6, beta=0.4):
        self.capacity = capacity
        self.alpha = alpha
        self.beta = beta
        self.buffer = []
        self.priorities = []

    def push(self, s, g, a, r, s2, g2, done):
        if len(self.buffer) < self.capacity:
            self.buffer.append((s, g, a, r, s2, g2, done))
            self.priorities.append(1.0)
        else:
            self.buffer.pop(0)
            self.priorities.pop(0)
            self.buffer.append((s, g, a, r, s2, g2, done))
            self.priorities.append(1.0)

    def sample(self, batch_size):
        probs = np.array(self.priorities) ** self.alpha
        probs /= probs.sum()
        idxs = np.random.choice(len(self.buffer), batch_size, p=probs)
        samples = [self.buffer[i] for i in idxs]

        weights = (len(self.buffer) * probs[idxs]) ** (-self.beta)
        weights /= weights.max()

        return (*zip(*samples), weights, idxs)

    def update_priorities(self, idxs, priorities):
        for i, p in zip(idxs, priorities):
            self.priorities[i] = max(p, 1e-6)

    def __len__(self):
        return len(self.buffer)

def check_goal(s, g):
    return set(s) == set(g)

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
        shape = func(shape.copy())
    return shape

def chamfer_distance_hex(shape1, shape2):
    if not shape1 and not shape2:
        return 0.0
    if not shape1 or not shape2:
        return 10.0

    def hex_dist(a, b):
        return abs(a[0]-b[0]) + abs(a[1]-b[1])

    d1 = sum(min(hex_dist(a,b) for b in shape2) for a in shape1) / len(shape1)
    d2 = sum(min(hex_dist(a,b) for a in shape1) for b in shape2) / len(shape2)
    return d1 + d2

def get_reward(prev, curr, goal):
    return chamfer_distance_hex(prev, goal) - chamfer_distance_hex(curr, goal)

def dqn_learning(filename, goal_states, episodes,
                 lr=1e-4, gamma=0.99, epsilon_start=1.0, epsilon_end=0.1,
                 exploration_fraction=0.8, max_steps=10, batch_size=32,
                 target_update_freq=1000, learning_starts=1000, train_freq=4):

    dqn = DQN(ACTION_SIZE).to(device)
    target = DQN(ACTION_SIZE).to(device)
    target.load_state_dict(dqn.state_dict())

    optimizer = optim.Adam(dqn.parameters(), lr=lr)
    loss_fn = nn.SmoothL1Loss()
    buffer = PrioritizedReplayBuffer()

    total_steps = episodes * max_steps
    exploration_steps = int(exploration_fraction * total_steps)

    global_step = 0

    for ep in tqdm(range(episodes)):
        state = []
        goal = goal_states[ep]
        
        # --- HER 改动 1: 增加临时轨迹缓存 ---
        episode_trajectory = [] 

        for t in range(max_steps):
            epsilon = max(
                epsilon_end,
                epsilon_start + (epsilon_end - epsilon_start) * global_step / exploration_steps
            )

            x = state_goal_to_tensor(state, goal).unsqueeze(0)

            if random.random() < epsilon:
                action_idx = random.randrange(ACTION_SIZE)
            else:
                with torch.no_grad():
                    action_idx = dqn(x).argmax(1).item()

            action = ACTION_LIST[action_idx]
            # 复制一份 state 防止 ACTIONS 函数原地修改影响后续逻辑
            next_state = ACTIONS[action](state.copy())

            # 计算原始奖励
            reward = -0.5 if next_state == state else get_reward(state, next_state, goal)
            done = check_goal(next_state, goal)
            if done:
                reward += 20.0

            # 存入原始经验
            buffer.push(state, goal, action_idx, reward, next_state, goal, done)
            
            # --- HER 改动 2: 记录轨迹 ---
            episode_trajectory.append((state, action_idx, next_state))

            state = next_state
            global_step += 1

            # 训练逻辑
            if len(buffer) > learning_starts and global_step % train_freq == 0:
                (s_batch, g_batch, a_batch, r_batch, s2_batch, g2_batch, d_batch, w_batch, idxs) = buffer.sample(batch_size)

                s_t = torch.stack([state_goal_to_tensor(si, gi) for si, gi in zip(s_batch, g_batch)])
                s2_t = torch.stack([state_goal_to_tensor(si, gi) for si, gi in zip(s2_batch, g2_batch)])
                a_t = torch.LongTensor(a_batch).to(device)
                r_t = torch.FloatTensor(r_batch).to(device)
                d_t = torch.BoolTensor(d_batch).to(device)

                q = dqn(s_t).gather(1, a_t.unsqueeze(1)).squeeze()

                with torch.no_grad():
                    next_a = dqn(s2_t).argmax(1)
                    next_q = target(s2_t).gather(1, next_a.unsqueeze(1)).squeeze()
                    target_q = r_t + gamma * next_q * (~d_t)

                loss = loss_fn(q, target_q)

                optimizer.zero_grad()
                loss.backward()
                optimizer.step()

                td = (q - target_q).abs().detach().cpu().numpy()
                buffer.update_priorities(idxs, td)

            if global_step % target_update_freq == 0:
                target.load_state_dict(dqn.state_dict())

            if done:
                break

        # --- HER 改动 3: Episode 结束后的事后重标记 (Hindsight Re-labeling) ---
        # 我们选取当前 Episode 最终达到的状态作为“虚假目标”
        if len(episode_trajectory) > 0:
            achieved_goal = episode_trajectory[-1][2] # 轨迹中最后一个 next_state
            
            # 只有当最终达到的状态不是空，且和原始目标不一样时，这种“学习”才有意义
            if achieved_goal and not check_goal(achieved_goal, goal):
                for s_her, a_idx_her, s_next_her in episode_trajectory:
                    # 针对这个“新目标”重新计算奖励
                    # 如果达到了这个虚假目标，new_done 就是 True
                    new_done = check_goal(s_next_her, achieved_goal)
                    new_reward = -0.5 if s_next_her == s_her else get_reward(s_her, s_next_her, achieved_goal)
                    if new_done:
                        new_reward += 20.0
                    
                    # 将“如果我的目标是 achieved_goal，我这步操作就很棒”的逻辑存入 Buffer
                    buffer.push(s_her, achieved_goal, a_idx_her, new_reward, s_next_her, achieved_goal, new_done)

    torch.save(dqn.state_dict(), filename)

def dqn_inference(model_path, goal, max_steps=10):
    dqn = DQN(ACTION_SIZE).to(device)
    dqn.load_state_dict(torch.load(model_path, map_location=device))
    dqn.eval()

    state = []
    actions = []

    for _ in range(max_steps):
        x = state_goal_to_tensor(state, goal).unsqueeze(0)
        with torch.no_grad():
            a = dqn(x).argmax(1).item()
        action = ACTION_LIST[a]
        state = ACTIONS[action](state.copy())
        actions.append(action)
        if check_goal(state, goal):
            break

    return actions

def main():
    MODE = "test"

    if MODE == "train":
        with open("train_shapes_10000_6.json", 'r') as f:
            all_goals = json.load(f)
        
        formatted_goals = []
        for g in all_goals:
            formatted_goals.append([tuple(coord) for coord in g])

        dqn_learning("dqn_shape_model.pth", formatted_goals, 10000)

    else:
        test_goal = get_shape_from_sequence(convert_string("ZXDR"))
        
        actions_taken = dqn_inference("dqn_shape_model.pth", test_goal)
        
        print(actions_taken)
        
        current_state = []
        for act in actions_taken:
            current_state = ACTIONS[act](current_state)
        
        if check_goal(current_state, test_goal):
            print("Sucess!")
        else:
            print("Failed!")

def batch_process_log_likelihood(model_path, input_json, output_json, temperature=1.0):
    with open(input_json, 'r') as f:
        sequences_raw = json.load(f)

    results = []
    
    dqn = DQN(ACTION_SIZE).to(device)
    dqn.load_state_dict(torch.load(model_path, map_location=device))
    dqn.eval()

    for seq_item in tqdm(sequences_raw):
            
        target_goal = get_shape_from_sequence(convert_string(seq_item))  
        current_state = []
        total_ll = 0.0
        
        for action_char in convert_string(seq_item):
            if action_char not in ACTION_TO_IDX:
                continue
                
            # 准备 Tensor
            x = state_goal_to_tensor(current_state, target_goal).unsqueeze(0)
            
            with torch.no_grad():
                q_values = dqn(x)
                log_probs = F.log_softmax(q_values / temperature, dim=1)
                
                idx = ACTION_TO_IDX[action_char]
                total_ll += log_probs[0, idx].item()
                
                current_state = ACTIONS[action_char](current_state.copy())
        
        results.append(total_ll)

    with open(output_json, 'w') as f:
        json.dump(results, f, indent=4)

batch_process_log_likelihood(model_path="dqn_shape_model.pth", input_json="sequences_new.json", output_json="log_likelihood_dqn.json",temperature=1.0)
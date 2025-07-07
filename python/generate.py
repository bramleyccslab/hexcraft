import random
import re
from collections import defaultdict

def parse_grammar_string(grammar_str):
    rules = defaultdict(list)
    for line in grammar_str.strip().splitlines():
        if not line.strip():
            continue
        lhs, rhs_part = line.split("->")
        lhs = lhs.strip()
        rhs_options = rhs_part.split("|")
        for option in rhs_options:
            match = re.match(r"(.*)\[(.*)\]", option.strip())
            if not match:
                continue
            rhs_symbols = match.group(1).strip().split()
            weight = float(match.group(2).strip())
            rules[lhs].append((rhs_symbols, weight))
    return rules

def generate_sentence(rules, start_symbol="S", max_depth=50, max_length=20):
    def expand(symbol, depth=0):
        nonlocal token_count
        if depth > max_depth or token_count >= max_length:
            return []
        if symbol.startswith("'") and symbol.endswith("'"):
            token = symbol.strip("'")
            token_count += 1
            return [token]
        if symbol not in rules:
            return [symbol]
        
        rhs_options = []
        adjusted_weights = []
        for rhs, weight in rules[symbol]:
            rhs_len = sum(1 for s in rhs if s.startswith("'"))
            penalty = 1.0 if token_count + rhs_len <= max_length else 0.1
            rhs_options.append(rhs)
            adjusted_weights.append(weight * penalty)

        if sum(adjusted_weights) == 0:  # fallback，避免权重全为0
            adjusted_weights = [w for _, w in rules[symbol]]

        chosen_rhs = random.choices(rhs_options, weights=adjusted_weights)[0]
        result = []
        for sym in chosen_rhs:
            result.extend(expand(sym, depth + 1))
            if token_count >= max_length:
                break
        return result

    token_count = 0
    return expand(start_symbol)

grammar_text = """
S -> S S [0.5] | A [0.5]
A -> 'a' [0.07692307692307693] | 'b' [0.07692307692307693] | 'c' [0.07692307692307693] | 'a' 'b' 'c' [0.7692307692307693]
"""

rules = parse_grammar_string(grammar_text)
for _ in range(10):
    sentence = generate_sentence(rules, max_length=10)
    print("".join(sentence))
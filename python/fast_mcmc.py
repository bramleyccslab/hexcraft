from nltk.parse import ChartParser
import random
import math
from tqdm.auto import tqdm
import copy
from functools import lru_cache
from generate_probabilistic import PCFG
from multiprocessing import Pool, cpu_count
import multiprocessing

def clean_text(s):
    # clean "... [xx]" to (..., xx)
    text_list = s.split("[")
    text1 = text_list[0].strip()
    text2 = text_list[1].strip(']').strip()
    return text1, float(text2)

def normalize_dict(rules):
    total = sum(rules.values())
    normalized_rules = {k: v / total for k, v in rules.items()}

    a_rules = []
    for rule, prob in normalized_rules.items():
        rhs = rule.split("->")[1].strip()
        a_rules.append(f"{rhs} [{prob}]")

    a_rule_str = "A -> " + " | ".join(a_rules)

    full_grammar = "S -> S S [0.5] | A [0.5]\n" + a_rule_str

    return full_grammar

def extract_terminals(rule_dict):
    mapping = {}
    for rule, value in rule_dict.items():
        rhs = rule.split("->")[1].strip()
        symbols = rhs.split()
        word = ''.join(s.strip("'") for s in symbols)
        mapping[word] = value
    return mapping

def parse_target_string(target_rule):
    rhs = target_rule.split("->")[1].strip()
    symbols = rhs.split()
    return ''.join(s.strip("'") for s in symbols)

def compute_probability(target, dictionary, p):
    total_weight = sum(dictionary.values())
    vocab = set(dictionary.keys())

    @lru_cache(None)
    def dfs(pos):
        if pos == len(target):
            return [([], 1.0)]

        results = []
        for end in range(pos + 1, len(target) + 1):
            word = target[pos:end]
            if word in vocab:
                weight_prob = dictionary[word] / total_weight
                rest_paths = dfs(end)
                for rest_seq, rest_prob in rest_paths:
                    results.append(([word] + rest_seq, weight_prob * rest_prob))
        return results

    all_paths = dfs(0)
    final_prob = 0.0
    for path, prob in all_paths:
        if len(path) >= 2:
            control_prob = (p ** (len(path) - 2)) * (1 - p)
            final_prob += prob * control_prob

    return final_prob

def prior(grammar, param_one=4, param_two=4):
    # prior distribution: we choose exponential (log probability, unnormalized)
    max_length = 0
    for rule in grammar.keys():
        count = len(rule.split("->")[1].strip().split())
        max_length = max(max_length, count)
    return - (param_one * len(grammar)) - (param_two * max_length)

_PARSER = None
_PROBS_DICT = None

def _worker_init(grammar_str):
    global _PARSER, _PROBS_DICT
    pcfg = PCFG.fromstring(grammar_str)
    _PARSER = ChartParser(pcfg)
    _PROBS_DICT = {}
    for prod in pcfg.productions():
        key, val = clean_text(str(prod))
        _PROBS_DICT[key] = val

def _worker_sentence_logprob(sentence):
    global _PARSER, _PROBS_DICT

    sentence_total_probability = 0.0
    for tree in _PARSER.parse(sentence):
        prob = 1.0
        for item in tree.productions():
            prob *= _PROBS_DICT[str(item)]
        sentence_total_probability += prob

    return math.log(sentence_total_probability)

def likelihood(grammar, sentences, num_workers=None):
    grammar_str = normalize_dict(grammar)
    if num_workers is None:
        num_workers = max(1, cpu_count() - 1)

    with Pool(processes=num_workers, initializer=_worker_init, initargs=(grammar_str,)) as pool:
        results = pool.map(_worker_sentence_logprob, sentences)

    total_logprob = sum(results)
    return total_logprob

def proposal(grammar, num_primitives, resample_parameter=0.4):
    choice = random.choices(["add", "delete"], weights=[num_primitives, sum(grammar.values()) - num_primitives])
    current_grammar = copy.deepcopy(grammar)

    if choice == ["add"]:
        bag = []
        for rule in grammar.keys():
            bag += [rule] * current_grammar[rule]

        samples = [random.choice(bag), random.choice(list(bag))]

        while random.random() < resample_parameter:
            samples.append(random.choice(bag))

        result = "A -> " + " ".join(rule.split(" -> ")[1] for rule in samples)
        prob = compute_probability(parse_target_string(result), extract_terminals(current_grammar), resample_parameter)
        proposal_probability = math.log(num_primitives / sum(current_grammar.values())) + math.log(prob)

        if result in current_grammar:
            current_grammar[result] += 1
        else:
            current_grammar[result] = 1

        reverse_probability = math.log(current_grammar[result] / sum(current_grammar.values()))

    elif choice == ["delete"]:
        eligible_options = [rule for rule in current_grammar.keys() if len(rule.split("->")[1].strip().split()) > 1]
        bag = []
        for rule in eligible_options:
            bag += [rule] * current_grammar[rule]

        option_to_remove = random.choice(bag)

        if current_grammar[option_to_remove] > 1:
            new_grammar = {k: (v - 1 if k == option_to_remove else v) for k, v in current_grammar.items()}
        elif current_grammar[option_to_remove] == 1:
            new_grammar = {k: v for k, v in current_grammar.items() if k != option_to_remove}

        proposal_probability = math.log(current_grammar[option_to_remove] / sum(current_grammar.values()))
        current_grammar = new_grammar

        prob = compute_probability(parse_target_string(option_to_remove), extract_terminals(current_grammar), resample_parameter)
        reverse_probability = math.log(num_primitives / sum(current_grammar.values())) + math.log(prob)

    return current_grammar, proposal_probability, reverse_probability

def main():
    initial_grammar = {"A -> 'a'": 1, "A -> 'd'": 1, "A -> 'z'": 1, "A -> 'x'": 1, "A -> 'w'": 1,
                       "A -> 'e'": 1, "A -> 's'": 1, "A -> 'f'": 1, "A -> 'r'": 1, "A -> 'k'": 1}

    sentences = ['ZKERRASFF', 'ZFEFRR', 'ZZFKF', 'ZSKKKKFFF', 'ZE', 'ZEEKEZ', 'ZFFFEEZF', 'ZEEKEZR', 'XDDXDZ', 'ZKKEEKK', 'XZ', 'ZEWA', 'ZEW', 'AEWWW', 'AEWWW', 'AEAEESAEA', 'AEAEEAEAR', 'AEAEEAEA', 'ZDSS', 'Z', 'ZKK', 'ZSSS', 'ZWWEEEZE', 'ZSSKKKFFF']
    sentences = [s.lower() for s in sentences]
    sentences = [list(sentence) for sentence in sentences]

    num_primitives = 10
    t = 1000
    grammar = initial_grammar

    likelihood_cache = {}

    sample_results = {}

    grammar_str_key = str(grammar)
    likelihood_cache[grammar_str_key] = likelihood(grammar, sentences)

    for _ in tqdm(range(t)):
        prior_p = prior(grammar)

        grammar_str_key = str(grammar)
        likelihood_p = likelihood_cache.get(grammar_str_key)
        if likelihood_p is None:
            likelihood_p = likelihood(grammar, sentences)
            likelihood_cache[grammar_str_key] = likelihood_p

        print(grammar)
        new_grammar, proposal_probability, reverse_probability = proposal(grammar, num_primitives)

        new_key = str(new_grammar)
        if new_key in likelihood_cache:
            new_likelihood_p = likelihood_cache[new_key]
        else:
            new_likelihood_p = likelihood(new_grammar, sentences)
            likelihood_cache[new_key] = new_likelihood_p

        new_prior_p = prior(new_grammar)

        log_acceptance_p = min(0, new_prior_p + new_likelihood_p + reverse_probability - prior_p - likelihood_p - proposal_probability)
        acceptance_p = math.exp(log_acceptance_p)

        a = random.uniform(0, 1)
        if a < acceptance_p:
            grammar = new_grammar

        if str(grammar) not in sample_results:
            sample_results[str(grammar)] = 1
        else:
            sample_results[str(grammar)] += 1

    result_dict = dict(sorted(sample_results.items(), key=lambda item: item[1], reverse=True))

    print(result_dict.values())
    print(list(result_dict.keys())[0])
    print(list(result_dict.keys())[1])
    print(list(result_dict.keys())[2])

if __name__ == "__main__":
    main()
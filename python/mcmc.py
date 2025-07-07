from nltk import PCFG
from nltk.parse import ChartParser
import random
import math
from tqdm.auto import tqdm
import copy
from functools import lru_cache
from nltk.parse.generate import generate

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
    # prior distribution: we choose exponential
    # log probability
    # for simplicity we don't normalize the prior probabilities

    # the length of the longest rule in the grammar
    max_length = 0

    for rule in grammar.keys():
        count = len(rule.split("->")[1].strip().split())
        max_length = max(max_length, count)

    return - (param_one * len(grammar)) - (param_two * max_length)

def likelihood(grammar, sentences):
    # likelihood: probability of sequence given the library
    parser = ChartParser(PCFG.fromstring(normalize_dict(grammar)))
    
    probs_dict = {}
    for probs in PCFG.fromstring(normalize_dict(grammar)).productions():
        clean_probs = clean_text(str(probs))
        probs_dict[clean_probs[0]] = clean_probs[1]

    log_probability = 0
    for sentence in sentences:
        sentence_total_probability = 0
        for tree in parser.parse(sentence):
            probability = 1
            for item in tree.productions():
                probability *= probs_dict[str(item)]
            sentence_total_probability += probability

        log_sentence_total_probability = math.log(sentence_total_probability)
        log_probability += log_sentence_total_probability

    return log_probability

def proposal(grammar, num_primitives, resample_parameter=0.4):
    # propose distribution: to modify the grammar
    # returns the sampled new grammar, and the forward/reverse probability
    # for simplicity, in our proposal we don't consider counts; we leave the different probabilities to the likelihood computation
    # here we ensure that there will be things to delete when we choose to delete
    # count of primitives (or basic primitives) are always 1
    choice = random.choices(["add", "delete"], weights = [num_primitives, sum(grammar.values()) - num_primitives])

    # dictionaries are mutable in Python, so we need to copy them to prevent leaking
    current_grammar = copy.deepcopy(grammar)

    if choice == ["add"]:
        # in order to prevent the irreversibility problem, we choose to concatenate multiple rules together (can be two or more)
        bag = []
        for rule in grammar.keys():
            bag += [rule] * current_grammar[rule]

        samples = [random.choice(bag), random.choice(list(bag))]
        
        while random.random() < resample_parameter:
            samples.append(random.choice(bag))

        result = "A -> " + " ".join(rule.split(" -> ")[1] for rule in samples)

        # the overall probability of getting an output added rule given an input grammar
        prob = compute_probability(parse_target_string(result), extract_terminals(current_grammar), resample_parameter)
        proposal_probability = math.log(num_primitives / sum(current_grammar.values())) + math.log(prob)

        if result in current_grammar:
            current_grammar[result] += 1
        else:
            current_grammar[result] = 1

        # probability of delete over add, multiplied by that of choosing one rule to delete
        reverse_probability = math.log(current_grammar[result] / sum(current_grammar.values()))

    elif choice == ["delete"]:
        # find all options with length > 1 (not primitives)
        # we're guaranteed that there must be at least one if we choose to delete
        eligible_options = [rule for rule in current_grammar.keys() if len(rule.split("->")[1].strip().split()) > 1]

        # We choose the option to remove based on their counts
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
    # the main function that carries out Metropolis-Hastings
    # step 1: initialize the grammar (the library) and the sequence
    initial_grammar = {"A -> 'l'": 1, "A -> 'f'": 1, "A -> 'm'": 1, "A -> 'r'": 1}
    sentences = [['l','r','m','l','r'], ['l','r','l','r'], ['l','r','m','l','r'], ['l','r','r','r'], ['l','f'], ['l','f','m','l'], ['l','f','m','l'], ['l','f','m','l'], ['l','r','r','r'], ['l','f'], ['l','r','m','l','r','m','l'], ['l','r','m','l','r','m','l'], ['l','r','r','m','l','r','m','l'], ['l','r','m','l','f','m','l']]
    num_primitives = 4

    # step 2: loop
    # sample a new grammar and evaluate probabilities
    # accept or reject
    t = 1000
    grammar = initial_grammar
    sample_results = {}
    for _ in tqdm(range(t)):
        prior_p = prior(grammar)
        likelihood_p = likelihood(grammar, sentences)

        print(grammar)
        new_grammar, proposal_probability, reverse_probability = proposal(grammar, num_primitives)
        new_prior_p = prior(new_grammar)
        new_likelihood_p = likelihood(new_grammar, sentences)

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


if __name__ == "__main__":
    main()

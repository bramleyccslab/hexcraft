from nltk.parse import ChartParser
import random
import math
from tqdm.auto import tqdm
import copy
from functools import lru_cache
from generate_probabilistic import PCFG

def clean_text(s):
    # clean "... [xx]" to (..., xx)
    text_list = s.split("[")
    text1 = text_list[0].strip()
    text2 = text_list[1].strip(']').strip()
    return text1, float(text2)


def normalize_dict(rules):
    """
    Convert grammar dict (rule_str -> count) to a PCFG string.
    MODIFICATION: now S -> S S and S -> A probabilities are taken from
    grammar counts if provided; otherwise default to 0.5 each.
    Also normalize A-rules from counts.
    """
    # sum counts for A rules only
    a_rules = []
    a_counts = {}
    for rule, cnt in rules.items():
        lhs, rhs = rule.split("->")
        lhs = lhs.strip()
        rhs = rhs.strip()
        if lhs == 'A':
            a_counts[rule] = cnt

    total_a = sum(a_counts.values()) if a_counts else 1.0
    normalized = {r: (c / total_a) for r, c in a_counts.items()}

    a_rule_items = []
    for rule, prob in normalized.items():
        rhs = rule.split("->")[1].strip()
        a_rule_items.append(f"{rhs} [{prob}]")

    a_rule_str = "A -> " + " | ".join(a_rule_items) if a_rule_items else "A -> 'a' [1.0]"

    # S rules: check if grammar provided counts for them; else default 0.5/0.5
    s_ss_count = rules.get("S -> S S", None)
    s_a_count = rules.get("S -> A", None)
    if s_ss_count is not None or s_a_count is not None:
        # if either provided, use their normalized counts
        s_ss = s_ss_count or 0
        s_a = s_a_count or 0
        total_s = s_ss + s_a if (s_ss + s_a) > 0 else 1.0
        p_ss = s_ss / total_s
        p_sa = s_a / total_s
    else:
        p_ss = 0.5
        p_sa = 0.5

    full_grammar = f"S -> S S [{p_ss}] | A [{p_sa}]\n" + a_rule_str
    return full_grammar


def extract_terminals(rule_dict):
    """
    MODIFICATION: only extract primitives of the form "A -> 'x'" where RHS
    is a single quoted terminal. Returns mapping terminal -> count.
    """
    mapping = {}
    for rule, value in rule_dict.items():
        lhs, rhs = rule.split("->")
        lhs = lhs.strip(); rhs = rhs.strip()
        # primitive if rhs is a single quoted terminal, e.g. "'a'"
        symbols = rhs.split()
        if len(symbols) == 1 and symbols[0].startswith("'") and symbols[0].endswith("'"):
            word = symbols[0].strip("'")
            mapping[word] = mapping.get(word, 0) + value
    return mapping


def parse_target_string(target_rule):
    rhs = target_rule.split("->")[1].strip()
    symbols = rhs.split()
    return ''.join(s.strip("'") for s in symbols)


def compute_probability(target, dictionary, p):
    """
    Base distribution H(f): probability of generating fragment `target`
    by concatenating primitives from `dictionary` with geometric length weight.
    This is kept from your original implementation (modest changes for clarity).
    """
    total_weight = sum(dictionary.values())
    if total_weight == 0:
        return 0.0
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


# -------------------- NEW: DP-style prior (CRP marginal) ----------------------

def prior_dp(grammar, alpha=1.0, primitives=None, p_resample=0.4):
    """
    Compute log prior for the grammar using a Dirichlet Process (CRP) style
    marginalized formula (marginal over seating given cluster sizes) and
    including the base distribution H(f) for fragment identities.

    p(G) = alpha^K * Gamma(alpha) / Gamma(alpha+N) * prod_i Gamma(n_i) * prod_i H(f_i)
    We return log p(G).

    - grammar: dict rule_str -> count
    - primitives: dict primitive terminal -> count (used to compute H)
    - p_resample: parameter passed to compute_probability for H
    """
    # collect fragment counts (we treat all grammar entries as "fragments")
    counts = []
    fragment_rules = []
    for rule, cnt in grammar.items():
        # ignore S -> ... meta rules from prior counting if present
        if rule.startswith('S ->'):
            continue
        counts.append(cnt)
        fragment_rules.append(rule)

    N = sum(counts)
    K = len(counts)
    if N == 0:
        return 0.0

    logp = K * math.log(alpha) + math.lgamma(alpha) - math.lgamma(alpha + N)
    # sum log Gamma(n_i)
    for n in counts:
        logp += math.lgamma(n)

    # include base distribution H for each unique fragment identity
    if primitives is None:
        primitives = {}

    for rule in fragment_rules:
        f = parse_target_string(rule)
        h = compute_probability(f, primitives, p_resample)
        # avoid log(0)
        if h <= 0:
            # give a very small probability instead of -inf
            logp += math.log(1e-300)
        else:
            logp += math.log(h)

    return logp


# -------------------- Likelihood (log-space, numeric stable) -----------------

def logsumexp(logs):
    if not logs:
        return float('-inf')
    m = max(logs)
    s = sum(math.exp(x - m) for x in logs)
    return m + math.log(s)


def likelihood(grammar, sentences):
    """
    Compute log likelihood of the data given the grammar.

    MODIFICATION: compute log-probabilities of parse trees and use log-sum-exp
    over trees for numeric stability. We still use NLTK's ChartParser to get
    possible parses, but we avoid direct multiplication of small floats.
    """
    # build PCFG from grammar counts
    grammar_str = normalize_dict(grammar)
    parser = ChartParser(PCFG.fromstring(grammar_str))

    # extract rule probabilities from PCFG object
    probs_dict = {}
    for probs in PCFG.fromstring(grammar_str).productions():
        clean_probs = clean_text(str(probs))
        # clean_probs[0] is something like "A -> 'a'"
        probs_dict[clean_probs[0]] = clean_probs[1]

    log_probability = 0.0
    for sentence in sentences:
        # sentence is list of terminals
        # get all parse trees (may be many) and compute log-prob for each
        tree_log_probs = []
        parses = list(parser.parse(sentence))
        if not parses:
            # if no parse, assign very small probability (log-domain)
            log_probability += math.log(1e-300)
            continue

        for tree in parses:
            logp = 0.0
            for item in tree.productions():
                prod_str = str(item)
                # the production string format matches the keys we put in probs_dict
                prob = probs_dict.get(prod_str, 1e-300)
                # avoid log(0)
                if prob <= 0:
                    logp += math.log(1e-300)
                else:
                    logp += math.log(prob)
            tree_log_probs.append(logp)

        # log P(sentence) = logsumexp(log probs of trees)
        log_sentence = logsumexp(tree_log_probs)
        log_probability += log_sentence

    return log_probability


# -------------------- Proposal (keeps use of H) -------------------------------

def proposal(grammar, num_primitives, resample_parameter=0.4):
    """
    Add/Delete proposal similar to original code.
    MODIFICATION: unchanged in structure, but we keep clarity that compute_probability
    is the base H used in forward/reverse probability calculations.
    """
    choice = random.choices(["add", "delete"], weights=[num_primitives, sum(grammar.values()) - num_primitives])

    current_grammar = copy.deepcopy(grammar)

    if choice == ["add"]:
        # bag of rules weighted by counts
        bag = []
        for rule in grammar.keys():
            bag += [rule] * current_grammar[rule]

        samples = [random.choice(bag), random.choice(list(bag))]
        while random.random() < resample_parameter:
            samples.append(random.choice(bag))

        # construct new composite rule
        result = "A -> " + " ".join(rule.split(" -> ")[1] for rule in samples)

        # proposal probability uses H(result)
        prob = compute_probability(parse_target_string(result), extract_terminals(current_grammar), resample_parameter)
        proposal_probability = math.log(num_primitives / sum(current_grammar.values())) + math.log(prob + 1e-300)

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
        reverse_probability = math.log(num_primitives / sum(current_grammar.values())) + math.log(prob + 1e-300)

    return current_grammar, proposal_probability, reverse_probability


# -------------------- Main (uses prior_dp) ----------------------------------

def main():
    print("participant 0")

    # initial primitive grammar (counts)
    initial_grammar = {"A -> 'a'": 1, "A -> 'd'": 1, "A -> 'z'": 1, "A -> 'x'": 1, "A -> 'w'": 1,
                       "A -> 'e'": 1, "A -> 's'": 1, "A -> 'f'": 1, "A -> 'r'": 1, "A -> 'k'": 1}

    sentences = ["WEFRERR", "FERRR", "EDFRE", "AWFREFRER"]
    sentences = [s.lower() for s in sentences]
    sentences = [list(sentence) for sentence in sentences]

    num_primitives = 10
    t = 1000
    grammar = initial_grammar
    sample_results = {}

    # precompute primitives counts for H computation
    primitives = extract_terminals(grammar)

    alpha = 1.0  # concentration parameter for DP prior; MODIFICATION: new hyperparam
    for _ in tqdm(range(t)):
        prior_p = prior_dp(grammar, alpha=alpha, primitives=primitives)
        likelihood_p = likelihood(grammar, sentences)

        # proposal
        new_grammar, proposal_probability, reverse_probability = proposal(grammar, num_primitives)

        # update primitives if new grammar changes primitive counts
        new_primitives = extract_terminals(new_grammar)

        new_prior_p = prior_dp(new_grammar, alpha=alpha, primitives=new_primitives)
        new_likelihood_p = likelihood(new_grammar, sentences)

        log_acceptance_p = min(0, new_prior_p + new_likelihood_p + reverse_probability - prior_p - likelihood_p - proposal_probability)
        acceptance_p = math.exp(log_acceptance_p)

        a = random.uniform(0, 1)
        if a < acceptance_p:
            grammar = new_grammar
            primitives = new_primitives

        sample_results[str(grammar)] = sample_results.get(str(grammar), 0) + 1

    result_dict = dict(sorted(sample_results.items(), key=lambda item: item[1], reverse=True))

    print(list(result_dict.values())[:10])
    print(list(result_dict.keys())[0])


if __name__ == "__main__":
    main()

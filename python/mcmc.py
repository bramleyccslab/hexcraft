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
    initial_grammar = {"A -> 'a'": 1, "A -> 'd'": 1, "A -> 'z'": 1, "A -> 'x'": 1, "A -> 'w'": 1, "A -> 'e'": 1, "A -> 's'": 1, "A -> 'f'": 1, "A -> 'r'": 1, "A -> 'k'": 1}
    # sentences = ['zwwwzkk', 'xkx', 'xwzkkkkkk', 'x', 'zfsxd', 'as', 'aeewx', 'zkez', 'xewzsr', 'zeswsz', 'zsr', 'zr', 'zsa', 'zwezkkkkkw', 'xzd', 'zwxws', 'zkkwzfwde', 'zd', 'zszkkkkkww', 'zkkkkezkkk', 'zzkkkxk', 'zeeawwex', 'xwsxwess', 'xweezws', 'awwees', 'zfffa', 'zf', 'zfs', 'zafsawes', 'awe', 'zfsxswed', 'xkafse', 'zxssezs', 'zkkkrfwee', 'zkkwezkkk', 'zsa', 'zwa', 'aa', 'zwzddaaddd', 'zw', 'zwa', 'xwzwes', 'aa', 'zkswazeee', 'zaskarr', 'zrrss', 'zxzx', 'z', 'zewewasss', 'azxzx', 'axeaasewwa', 'azess', 'xz', 'zxa', 'zxa', 'zx', 'fdes', 'zkkkkkka', 'xdawd', 'zkkkkkkxd', 'xwxkkkkkk', 'aawewxws', 'xwxdd', 'zwxw', 'aawawwaw', 'awsawaxw', 'zkswazkkk', 'xkksas', 'zkkkessa', 'zkkkrses', 'xwxwxes', 'zszewzwszw', 'zwzed', 'zkkwawzkkk', 'xwsxew', 'zsa', 'zrr', 'zkswzddzw', 'xkkkkkkk', 'aweeasww', 'zkswzdd', 'z', 'zkksddd', 'zxk', 'z', 'z', 'z', 'z', 'z', 'zxza', 'a', 'a', 'zkkwwszw', 'awwaweee', 'x', 's', 'zff', 'zxd', 'xwwdkk', 'xffaeessaa', 'xkaesaasas', 'zkswara', 'azx', 'azxd', 'zkwsra', 'zxweeweffr', 'xeessssss', 'zkkrkkks', 'xkx', 'aexw', 'zkswra', 'zkksdkxe', 'xwzw', 'zkkkkerkkr', 'xkzkkk', 'k', 'x', 'xzkxkzx', 'zkkrz', 'zrkz', 'x', 'xk', 'zkkkkkksa', 'zkkkkk', 'zwezkkkkkw', 'zkwsrkkkk', 'zkwsfff', 'zfz', 'zkkkkk', 'aeasaswawa', 'xkzwwe', 'zeekkkk', 'zzads', 'zsa', 'aaaasaasa', 'zssz', 'zszkkkkkww', 'zrrr', 'zkswarrew', 'zewz', 'zewxweear', 'zffrrr', 'awezfs', 'zfffrwwdd', 'xwxkkkkkdd', 'zrrrkfff', 'zwx', 'xwaeessww', 'zwweaw', 'aeweaeww', 'zksswwezez', 'zzz', 'zrrss', 'xseeew', 'xfkkk', 'xdzffd', 'zkkkkkww', 'zkws', 'xwszzakkkk', 'aawewew', 'axewwwf', 'azs', 'zsa', 'zkswzkkkk', 'asaswwka', 'x', 'zwea', 'zkswrddd', 'azkswaz', 'zkswfz', 'zezrwr', 'zwrw', 'zsa', 'zsxd', 'zsa', 'zszkkkkkr', 'zszkkkkkww', 'xkew', 'xskkks', 'zkkwrr', 'zf', 'zsa', 'aeak', 'awawawk', 'zszkkkkkww', 'aeweawss', 'xreaw', 'awxkkkrkra', 'xawr', 'xswa', 'zkkkwz', 'awaeassa', 'zrakkrw', 'azrkrssw', 'zzwrzzr', 'xaszx', 'xww', 'zseawexxz', 'xaxrd', 'xwwz', 'wazkazkkws', 'azkkkkszsa', 'zkkkwzkws', 'zszkkkkkww', 'zkwwwsssez', 'zr', 'zsrak', 'zswz', 'z', 'xdewexs', 're', 'zfsawe', 'zrsawe', 'aesasa', 'zkkksawedd', 'aaaaa', 'zkwzz', 'zkkkkkke', 'zszkkkkkww', 'zkwewssr', 'zzzzz', 'zkrff', 'zezrrkkas', 'zrkrsesd', 'xeaweaxw', 'xwerfzfw', 'zkkkewer', 'zsa', 'zkkwar', 'zkffrdd', 'zkkkkezk', 'awssaww', 'zkkkkzk', 'zkwsarddd', 'w', 'ase', 'xeewazws', 'zkkewfre', 'zkkw', 'aaweasawef', 'xawsexkrkk', 'zkswrx', 'azsa', 'xdzd', 'zkkkkezksw', 'assz', 'azz', 'zsa', 'zddd', 'xwzff', 'aafr', 'aedd', 'zswfsaww', 'zz', 'zez', 'zszkkkkkww', 'zkkkkerff', 'xdd', 'zeeea', 'zeeawwwxx', 'xssasawez', 'xexz', 'zeaw', 'zxaexdd', 'aeweeaeswz', 'zkkkwsesxd', 'zkkwewxs', 'zkswx', 'zesa', 'zkr', 'zr', 'zsaewer', 'zsaewer', 'zr', 'axesawewer', 'zrrekera', 'zxr', 'azxeawer', 'aexwrr', 'aexzr', 'xsxeswzw', 'xkzk', 'zkswrwd', 'zersawr', 'zeera', 'xk', 'zfekewr', 'xzsessrwd', 'xzssraes', 'xzsres', 'xzssresww', 'xzr', 'xzsesr', 'zxww', 'xra', 'awwxwesswk', 'x', 'zsa', 'zd', 'z', 'zweseww', 'zfffz', 'zwrf', 'zeff', 'zeewrf', 'x', 'zaweeaaff', 'ze', 'zffkkksxd', 'xkzdd', 'zkswrrddd', 'azskadd', 'azs', 'azsa', 'axwaxdd', 'axwxssfssa', 'a', 'a', 'azkffrr', 'azkrr', 'zweewxfes', 'zkeezkkes', 'zkkkeekke', 'xrkx', 'zkwsra', 'zkkkkkw', 'zkwsreee', 'zkkkkezee', 'zkkkker', 'zeesasxr', 'wzkkkx', 'ze', 'zkkkezew', 'zkkrzdd', 'zkkzkkkkkk', 'zkkw', 'kzkkkzkk', 'zwwzw', 'zwwwzw', 'zrkrw', 'xrkrkrw', 'xfrwa', 's', 'zxxxrf', 'z', 'd', 'x', 'aaw', 'a', 'zkkk', 'a', 'a', 'xsdewzsew', 'zxsewewdss', 'zkkkkaeawa', 'zszkkkkkww', 'xkzzk', 'xz', 'xewzskr', 'x', 'zkswr', 'xzxxz', 'xssx', 'xsawe', 'zfsaeswx', 'zxd', 'zewzkkkkkw', 'zesswrka', 'zkkkwerk', 'zkkzkkdd', 'zkkrkkkk', 'zkkff', 'xxkx', 'zkkkkwffrr', 'zkwsrffra', 'zrzrrzrz', 'xkzkkkkssk', 'zkkkkerkk', 'z', 'z', 'z', 'z', 'z', 'zrrrf', 'z', 'z', 'z', 'rz', 'z', 'z', 'z', 'z', 'z', 'z', 'z', 'zesswa', 'zkwesswra', 'xswxswewz', 'dxa', 'wea', 'wazwedxr', 'wxarw', 'x', 'wx', 'a', 'a', 'rz', 'edxxad', 'azawewerz', 'wzew', 'a', 'xa', 'axzr', 'wszrewafew', 'wwwrzw', 'aaszr', 'araed', 'azffrrf', 'azkkwar', 'axkazess', 'azkffrrr', 'azrrff', 'asdad', 'axkazw', 'awfrzkkw', 'aew', 'xzz', 'zrf', 'asswzsr', 'aaesszewsr', 'zsa', 'zas', 'awswzx', 'xr', 'zr', 'zr', 'zfr', 'zza', 'xwae', 'zzkkr', 'zz', 'zk', 'zkwsre', 'zkwszkkkkk', 'zkswr', 'zkswaa', 'zksw', 'zx', 'zr', 'xxzz', 'zxarr', 'zx', 'zxa', 'zxr', 'xzfrr', 'zx', 'azxx', 'x', 'zxxza', 'awssas', 'xz', 'xzr', 'xzd', 'aeewwxswz', 'aewwz', 'zewxwr', 'zewkkkkkwz', 'zewwr', 'zkswr', 'zkswzrkkk', 'zkkk', 'xwx', 'xk', 'zffffxd', 'awwsxw', 'zkkwas', 'zkkwzkkkkk', 'fzkkkkkwwk', 'zx', 'zxzxf', 'zxexweaw', 'xzd', 'zxeexwx', 'ezxxd', 'zxexsesadd', 'ezxwwxex', 'xzxsxerx', 'xszeexsxer', 'zxe', 'axfree', 'azwweesssa', 'axkkkkkk', 'axz', 'zfrrxrf', 'xsswewazw', 'zwxweswdd', 'zwxwa', 'zwxawzs', 'zwxwsde', 'xfredfrd', 'zweerff', 'zsa', 'zwraw', 'aewwzw', 'aez', 'aewzwr', 'zkkr', 'zfkkwwzf', 'zxewdesr', 'zkswar', 'zskaffrr', 'asww', 'zwwr', 'zer', 'zr', 'zwksswarr', 'zssr', 'zxwsses', 'zsa', 'zwxwd', 'xwzkkk', 'aswsawd', 'zkkkkkzkkk', 'zkkkkkwwz', 'zkkkker', 'zzkswdr', 'zkkkke', 'awwr', 'zer', 'xdk', 'zswes', 'zxzaddd', 'xkx', 'xzweweassw', 'zkswar', 'zkswasa', 'zra', 'zerxweewkw', 'zwwddw', 'a', 'waasxzs', 'a', 'zsa', 'zfsrz', 'xwxsr', 'zrrrr', 'rfzfffsrrf', 'zrr', 'zkkkkkwwzk', 'zewwzw', 'zwzdwdezkk', 'zewwxwr', 'xfkakkkzrr', 'zrrrf', 'zkkkkerd', 'zkkkkezksw', 'zkkkkewze', 'zkkkkzz', 'awezkkdsw', 'xzd', 'zkkkkk', 'zkkkkkfr', 'as', 'aswasw', 'zkkkkkwzx', 'zz', 'xkwxkw', 'zwewzw', 'zwewrffr', 'zfsaawrwwd', 'awaweaawes', 'zkkeeaaa', 'xwzkkkkk', 'wawsawasww', 'awssw', 'aeassa', 'seaewesa', 'asswasw', 'xkzkkkkk', 'xwsr', 'zkwszkkkr', 'zrz', 'zkrk', 'aes', 'xkkk', 'xearrf', 'xkkkweew', 'asswaffr', 'as', 'ae', 'aeeeawsws', 'zkkkk', 'xrf', 'xsssxewe', 'xsssxssx', 'xsssxewe', 'xzw', 'azx', 'xk', 'x', 'xk', 'xkk', 'xksssswek', 'zk', 'xkks', 'k', 'xkke', 'xkkz', 'xsz', 'zkkwws', 'xsskdww', 'xwz', 'w', 'xxkw', 'xz', 'xkxkkk', 'xkx', 'x', 'xkxww', 'zkkkzw', 'zwees', 'zkkswwxz', 'zwwee', 'xkxrr', 'zfsxd', 'zkwsaw', 'xws', 'zkwszksss', 'zwxws', 'zkkkkkww', 'zkws', 'zkwszkkkxe', 'xseszkkkk', 'aa', 'zewzz', 'zz', 'zzwweed', 'zkkwswr', 'daaxaseeww', 'zessddkeed', 'zsa', 'xssad', 'xwreszw', 'zszewww', 'zszeww', 'zszewwd', 'zkkkkera', 'axxkf', 'zsxskkkzsk', 'zxsesr', 'awsszxsesr', 'zxses', 'ffdazr', 'zkwsxwae', 'zkkwazws', 'azrf', 'zsa', 'zwx', 'zkswa', 'zwweeex', 'zeaeaswwwx', 'xssez', 'zeeawxw', 'x', 'zex', 'zeex', 'zex', 'zwx', 'zsex', 'zsswwx', 'xex', 'xessz', 'zeeawwxeea', 'zeeaz', 'xeea', 'zeeawwxear', 'zkwex', 'aez', 'zxkkk', 'zkkswwxz', 'zkkswwxa', 'zkkkweexzd', 'zfxsfsd', 'zkkwr', 'zsr', 'zrsssa', 'ass', 'xewsewz', 'zkkkr', 'zfffffsxd', 'zeeeww', 'xexssxew', 'zkkzswwr', 'zeew', 'xr', 'xzw', 'xaweskx', 'xwxswe', 'xswxw', 'zxxaw', 'zr', 'zkkrw', 'a', 'zkkkkkww', 'zkkkkk', 'zwr', 'zwwewez', 'zkkkker', 'zsrakkkk', 'xeass', 'wew', 'zxwwsa', 'zkkwer', 'xzaweed', 'zd', 'zx', 'zxd', 'zxwexws', 'zszkkkkkww', 'zkwsr', 'zkwszwdwz', 'zssxszdkee', 'xkkkkwkz', 'zrkksszr', 'z', 'wsz']
    # sentences = [list(sentence) for sentence in sentences]
    num_primitives = 10
    grammar = PCFG.fromstring("""
    S -> S S [0.5] | A [0.5]
    A -> 'a' [0.08] | 'd' [0.08] | 'z' [0.08] | 'x' [0.08] | 'w' [0.08] | 'e' [0.08] | 's' [0.08] | 'f' [0.08] | 'r' [0.08] | 'k' [0.08] | 'z' 'x' 'd' 'r' [0.2]
    """)

    sentences = [sentence for sentence in grammar.generate(15)]
    sentences = [''.join(sentence.split(" ")) for sentence in sentences]
    sentences = [list(sentence) for sentence in sentences]

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

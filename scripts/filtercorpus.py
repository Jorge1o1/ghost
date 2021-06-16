ALLOWED_LETTERS = ['D','O','G','E']
MIN_LETTERS = 4

def uses_only_allowed(word):
    return all(l in ALLOWED_LETTERS for l in word.rstrip())

def is_longer_than_minimum(word):
    return len(word) > MIN_LETTERS

with open("../data/corpus.sample", "r") as infile:
    with open("../data/corpus.txt", "w") as outfile:
        outfile.writelines(filter(is_longer_than_minimum, filter(uses_only_allowed,infile)))
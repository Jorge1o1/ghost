ALLOWED_LETTERS = ['A','B','C','D','E']
MIN_LETTERS = 4

def uses_only_allowed(word):
    return all(l in ALLOWED_LETTERS for l in word.rstrip())

def is_longer_than_minimum(word):
    return len(word) > MIN_LETTERS

with open("../data/corpus.txt", "r") as infile:
    with open("../data/corpus.sample", "w") as outfile:
        outfile.writelines(filter(is_longer_than_minimum, filter(uses_only_allowed,infile)))
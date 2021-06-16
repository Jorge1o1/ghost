ALLOWED_LETTERS = ['D','O','G']

def uses_only_allowed(word):
    return all(l in ALLOWED_LETTERS for l in word.rstrip())

with open("corpus.sample", "r") as infile:
    with open("corpus.txt", "w") as outfile:
        outfile.writelines(filter(uses_only_allowed,infile))
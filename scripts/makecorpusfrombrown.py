MIN_LETTERS = 3
TOP_N = 5000

def get_count_from_line(line):
    parts = line.split()
    return int(parts[1]) if len(parts) > 1 else 0

def is_longer_than_minimum(line):
    word = line.split()[0]
    return len(word) > MIN_LETTERS

with open("../data/brown_freq.txt", "r") as infile:
    sorted_qty = sorted(infile, key=get_count_from_line, reverse=True)
    valid = filter(is_longer_than_minimum, sorted_qty[:TOP_N])
    sorted_alpha = [l.split()[0] + "\n" for l in sorted(valid)]
    with open("../data/corpus2.txt", "w") as outfile:
        outfile.writelines(sorted_alpha)
"""
The nth term of the sequence of triangle numbers is given by, tn = n(n+1)/2;
so the first ten triangle numbers are:

    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its
alphabetical position and adding these values we form a word value. For
example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value
is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
containing nearly two-thousand common English words, how many are triangle
words?
"""


alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'


def score_word(word):
    total = 0

    for c in word:
        total += alphabet.find(c) + 1

    return total


def T(n):
    return n*(n+1)/2


Ts = [T(n) for n in range(200)]


with open('/home/alex/Programming/euler/p042_words.txt') as f:
    raw = f.read()
    text = ''.join(c for c in raw if c != '"')
    words = text.split(',')

scores = [score_word(word) for word in words]
triangle_words = [score for score in scores if score in Ts]
print(len(triangle_words))

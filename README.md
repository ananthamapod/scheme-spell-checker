# Scheme-Spell-Checker 
Multiple hash based spell checker written in Scheme for Rutgers Computer Science Course, Principles of Programming Languages 198:314

## Implementation
Uses multiple hashes including division hashing and multiplication hashing as well as hybrids of the two - with several different seeds, and applies the hashes to a dictionary.
The hashes all have their own hash tables initially set to all zeros. When the hashes are applied to the words in the dictionary, for each word, the locations indexed by the hashes are set to one.

Then, the hash tables can now be applied to test words, and if there is a 1 corresponding to the location indexed by all of the hashes, the word is valid according to the dictionary.

## This code is licensed according to the MIT License.

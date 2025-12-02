#!/usr/bin/python3

# This script will convert a Nexus file for the
# visitor model with K current locations encoded
# as K binary characters (e.g. 01000 is current
# location 2 out of 5; or one-hot encoding) into
# a single character with K^2 compound states
# where the current location is observed across
# all unobserved (ambiguous) home locations
# (e.g. "01000" -> "(2 7 C I N")

# Example matrix
#
# Home location on row, away location on column.
# If sampled *in* location 2, home location is ambiguous,
# so we choose all states in column marked by *
#
#         *
#     0 1 2 3 4
#    ----------
# 0  |0 1 2 3 4
# 1  |5 6 7 8 9
# 2  |A B C D E
# 3  |F G H I J
# 4  |K L M N O
#         *

import sys
import re
import string

in_fn = sys.argv[1]
#symbols = sys.argv[2]

# conversion structures
num_loc = -1
state_str = ''
state_dict = {}

# open file
in_file = open(in_fn, 'r')

# parse file
s = ''
in_char_matrix = False
for line in in_file:
    line = line.lower()
    line = line.rstrip('\n')
    line = re.sub(r'\s+', ' ', line)

    # skip blank lines
    if line == '':
        continue

    # find number of locations
    if 'nchar' in line:
        toks = line.split(' ')
        for t in toks:
            if 'nchar' in t:
                num_loc = int( t.split('=')[-1].rstrip(';') )
                break
        
        # set up state space
        if num_loc > 0:
            num_states = num_loc ** 2
            for i in range(num_states):
                if i <= 9:
                    state_str += str(i)
                else:
                    state_str += string.ascii_uppercase[i-10]

            for i in range(num_loc):
                state_vec = [0] * num_loc
                state_vec[i] = 1
                state_key = ''.join([str(x) for x in state_vec])
                state_dict[state_key] = '(' + ' '.join(list(state_str[i::num_loc])) + ')'
        line = re.sub(r'nchar=[0-9]+', 'nchar=1', line)

    # format line w/ symbol string
    if 'format' in line:
        line = re.sub(r'symbols="[0-9A-Za-z]+"', f'symbols="{state_str}"', line)

    # process character matrix
    if in_char_matrix == True:
        # check if done processing matrix
        if line == ';':
            in_char_matrix = False
        # tokenize characters in matrix
        toks = line.split(' ')
        # remove useless white space tokens
        toks = [ x for x in toks if x != '' ]
        # update taxon data, if taxon exists as key in state dictionary
        if len(toks) == 2 and toks[1] in state_dict.keys():
            line = toks[0] + ' ' + state_dict[toks[1]]

    # switch mode to process character matrix
    if 'matrix' in line:
        print('in char matrix!')
        in_char_matrix = True

    s += line + '\n'

# write file
out_fn = in_fn.replace('.nex', '.ambig.nex')
out_file = open(out_fn, 'w')
out_file.write(s)
out_file.close()

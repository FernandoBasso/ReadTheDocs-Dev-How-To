#!/bin/bash

#
# Looping and Skipping
# ====================
#
# https://www.hackerrank.com/challenges/bash-tutorials---looping-and-skipping
#

#
# We could also use `$(seq 99)` instead of the range. It would be using another
# process and an external program, but it would work.
#
# Also,
#

#
# Print odd numbers from 1 to 99.
#
seq 1 2 99

#
# would start at 1, increment by 2, and stop on 99. With a range like
# `{1..99..2}`, this would do the same.
#
# And there is some more trickery that could be done. Note the space between \n
# and { in the first example and its output compared to the second example.
#

echo -e '\n' {1..9..2}
# → 1 3 5 7 9

echo -e '\n'{1..9..2}
# → 1
# → 3
# → 5
# → 7
# → 9

#
# Or using `printf` instead:
#

printf '%d\n' {1..9..2}
# → 1
# → 3
# → 5
# → 7
# → 9

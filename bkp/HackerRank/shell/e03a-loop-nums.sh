#!/bin/bash

#
# Looping With Numbers
# ====================
#
# https://www.hackerrank.com/challenges/bash-tutorials---looping-with-numbers
#

##
# Loops numbers from 1 to 50.
#
for num in {1..50} ; do
  printf '%d\n' "$num"
done
# → 1
# → 2
# .
# .
# .
# → 50

#
# We simply use a bash range to create a list of numbers and print each one of
# them.
#


#!/usr/bin/env bash

#
# Cut #1
# ======
#
# https://www.hackerrank.com/challenges/text-processing-cut-1
#

cut --bytes 3 -

#
# Just for fun, use sed (with GNU extension \U) to uppercase
# the characters.
#
cut --bytes 3 - | sed 's/./\U&/'


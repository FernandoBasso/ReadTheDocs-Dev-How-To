#!/bin/bash

#
# World of Numbers
# ================
#
# https://www.hackerrank.com/challenges/bash-tutorials---the-world-of-numbers/problem
#
# This one uses bc and shell expansion to perform the four operations in a
# single line!
#
# Running
# -------
#
# Either run the script and type each number followed by Enter or use a
# here-string:
#
#      bash script.sh <<<$'20\n5'
#

read -r x <&0
read -r y <&0

printf '%s\n' "$x"{+,-,*,/}"$y"| bc
# → 25
# → 15
# → 100
# → 4

#
# Note the use of `%s` (and not `%d`). `%s` works, but `%d` gives an error
# about “invalid number”.
#
# == Parenthesis
# --------------
#
# The parenthesis around $y, that is, "($y)" is to avoid problems in case
# y is negative. In bc alone, 3 - -2 or 3 + -2 works. printf 3{+,-}-2 works
# too. But the combination of printf and bc with that -2 causes problems.
# Placing -2 inside parenthesis makes everything work fine.
#
#
# == About STDIN
# --------------
#
# As noted in the previous file
#
#     /dev/stdin
#
# behaves differently than
#
#     <&0
#
# The former causes problems with here-strings like
#
#     bash script.sh <<<$'10\n5'
#
# while the latter does not.
#


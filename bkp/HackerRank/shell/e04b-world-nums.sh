#!bin/bash

#
# World of Numbers
# ================
#
# https://www.hackerrank.com/challenges/bash-tutorials---the-world-of-numbers/problem
#
# NOTE: This solution does not work...
#

#
# == Solution 1
# -------------
#
# One solution is to just invoke `read` twice!
#

read -r x < /dev/stdin
read -r y < /dev/stdin

# read x <&0
# read y <&0

printf 'x: %s\ny: %s\n' "$x" "$y"

printf '+ %d\n' $(( x + y ))
printf -- '- %d\n' $(( x - y ))
printf '* %d\n' $(( x * y ))
printf '/ %d\n' $(( x / y ))

#
# Running it like this does not work:
#
#     bash script.sh <<<$'20\n5'
#
# It is reading 20 into both x and y.
#
# → x: 20
# → y: 20
# → 40
# → 0
# → 400
# → 1

#
# 20 + 20 = 40
# 20 - 20 = 0
# 20 * 20 = 400
# 20 / 20 = 1
#
# TODO: What the poop is going on‽ Why the incorrect read thing‽
#

#
# Found The Problem!
# ------------------
#
# Turns out if we use <&0 instead of /dev/stdin to read input, then
# everything works as expected.
#


#!/bin/bash

#
# World of Numbers
# ================
#
# https://www.hackerrank.com/challenges/bash-tutorials---the-world-of-numbers/problem
#

#
# From -100 to 100, y != 0.
#
# The input comes in _two lines_ (not two parameters in a single line). That is
# important.
#
# That means we _cannot_ do something like this, because that two parameters in
# _one single line_:
#
#     read x y <<<'10 20'
#
# TIP: `read` is a shell built-in. Try `type read` and `help read`.
#
# Alas, `read` does not work in a "`multiline`" way:
#
#   “
#   Read a line from the standard input and split it into fields.
#   ”
#
# Or
#
#   “
#   Reads a single line from the standard input, or from file descriptor FD if
#   the -u option is supplied. The line is split into fields as with word
#   splitting, and the first word is assigned to the first NAME, the second
#   word to the second NAME, and so on, with any leftover words assigned to the
#   last NAME. Only the characters found in $IFS are recognized as word
#   delimiters.
#   ”
#
# See‽ “Read *a* line” and “Read a *single* line”. Very well emphasized!
#
# So, IFS=$'\n' (I mean, the $'\n' part) does not work with `read`. It
# simply does not read more than a line at once.
#
# This means something like this won't work:
#
#     # script file
#     while IFS=$'\n' read x y ; do
#       printf 'x: %s\n' $x
#       printf 'y: %s\n' $y
#     done <&0
#
#     # command line
#     bash script.sh <<<$'10\n20'
#     # → x: 10
#     # → y:
#     # → x: 20
#     # → y:
#
# It would loop twice (if we have two input lines), and assign the value to `x`
# each time, never assigning anything to `y`.
#
# STDIN
# -----
#
# To read from STDIN:
#
#     read myvar < /dev/stdin
#
# Or
#
#     read myvar <&0
#
# To conditionally read from either a file as the first argument or STDIN, we
# can use PARAMETER SUBSTITUTION. So, if a parameter with a file path is not
# passed, we default to STDIN.
#
#     read myvar "${1:-/dev/stdin}"
#
# == Solution 1
# -------------
#
# One solution is to just invoke `read` twice!

read -r x < /dev/stdin
read -r y < /dev/stdin

printf '%d\n' $(( x + y ))
printf '%d\n' $(( x - y ))
printf '%d\n' $(( x * y ))
printf '%d\n' $(( x / y ))

# bash script.sh <<<$'10\n20'
# → 20
# → 0
# → 100
# → 1
#

# == Solution 2
#
# This one uses shell expansion to perform the four operations in a single line!
#

# → read x <&0
# → read y <&0
# →
# → printf '%s\n' $x{+,-,*,/}$y | bc
#
# Note the use of `%s` (and not `%d`). `%s` works, but `%d` gives an error
# about “invalid number”.


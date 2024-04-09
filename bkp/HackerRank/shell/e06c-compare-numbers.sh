#!/bin/bash

: <<-'////'
--------------------------------------------------------------------------------
== Compare Numbers

https://www.hackerrank.com/challenges/bash-tutorials---comparing-numbers

Running:

    $ bash script.sh <<<$'3\n5'
    $ printf '%d\n' 3 5 | bash script.sh

--------------------------------------------------------------------------------
////

read -r x <&0
read -r y <&0

# Array of messages.
msgs=('X == ' 'X < Y' 'X > Y')

# Array of 0s and 1s as per bc's output for each operation
mapfile -t bools < <(printf '%s\n' "$x"{==,\<,\>}"$y" | bc)

# Calculate the index of the message.
idx=$(( "${bools[0]}" * 0 + "${bools[1]}" * 1 + "${bools[2]}" * 2 ))

# Prints the message.
printf '%s\n' "${msgs[$idx]}"


#
# Uses a system of “weights” to calculate the index of the message to display.
#
# Bash has this nice brace expansion feature:
#
#     $ printf '%s\n' '10 '{XX,YY,ZZ}' 20'
#     10 XX 20
#     10 YY 20
#     10 ZZ 20
#
# Which could also be:
#
#     $ printf '%s\n' '10 '{\<,\>,==}' 20'
#     10 < 20
#     10 > 20
#     10 == 20
#
# And `bc`:
#
#     $ echo '5 == 5' | bc
#     1
#
#     $ echo '5 > 5' | bc
#     0
#
#     $ echo '4 < 5' | bc
#     1
#
# 1 is true, 0 is false.
#
# Combining both:
#
#     $ printf '%s\n' '4'{\<,\>,==}'5' | bc
#     1
#     0
#     0
#
#     $ printf '%s\n' '5'{\<,\>,==}'4' | bc
#     0
#     1
#     0
#
#     $ printf '%s\n' '5'{\<,\>,==}'5' | bc
#     0
#     0
#     1
#
# Note how the 0s and 1s are arranged each time. We use that to our advantage and
# create an array with them:
#
#     $ mapfile -t bools < <(printf '%s\n' "$x"{==,\<,\>}"$y" | bc)
#
#     $ echo "${bools[@]}"
#     0 1 0
#
# Explaining this line:
#
#     idx=$(( "${bools[0]}" * 0 + "${bools[1]}" * 1 + "${bools[2]}" * 2 ))
#
# We are using some math trickery to calculate the index of the correct message
# based on the zeroes and ones of the `bools` array. We multiply each value of
# the array with its “weight”, or “index” value.
#
# The order of the opreations in the brace expansion: `==`, then `<`, then `>` is
# important because it is related to the order of the `bools` array elements and
# the `msgs` array.
#
#
# When `x == y`, 1 is at index 0 on `bools`.
#
#       1       0       0
#     0 * 0 + 0 * 1 + 0 * 2 = 0
#
# When `x < y`, 1 is at index 1 on `bools`.
#
#       0       1       0
#     0 * 0 + 0 * 1 + 0 * 2 = 1
#
# When `x > y`, 1 is at index 2 on `bools`.
#
#       0       0       1
#     0 * 0 + 0 * 0 + 1 * 2 = 2
#
#
# === References
#
# - `help mapfile`
# - `info bc` and `man bc`
# - https://www.gnu.org/software/bash/manual/bash.html#Brace-Expansion
# - https://mywiki.wooledge.org/BraceExpansion
#

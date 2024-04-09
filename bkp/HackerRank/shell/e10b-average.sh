#!/bin/bash

#
# Compute The Average
# =====================
#
#
# https://www.hackerrank.com/challenges/bash-tutorials---compute-the-average
#

# Produces shellcheck error.
# https://github.com/koalaman/shellcheck/wiki/SC2207
#   input=($(cat))
#

# Read input into array input.
mapfile -t input < <(cat)

# Turn array into digits string separated by spaces.
nums=${input[*]}

printf '%.3f\n' "$(echo "scale=4; (${nums// / + }) / ${#input[*]}" | bc -l)"

#
# Remember that printf %.f does rounding (man 1 printf and man 3 printf).
# Therefore, use scale=4 (not 3) in bc.
#
# Replace spaces with ' + ' which can then be fed to bc.
#


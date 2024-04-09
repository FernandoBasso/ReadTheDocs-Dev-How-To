#!/usr/bin/env bash

#
# = Compute The Average
# =====================
#
# https://www.hackerrank.com/challenges/bash-tutorials---compute-the-average
#

read -r n
sum=0

echo $sum

if [[ "$n" == 0 ]]
then
  printf '%.3f\n' "$(echo 'scale=4; 0' | bc -l)"
  exit 0
fi

for ((i = 0; i < n; ++i))
do
  read -r x
  sum=$((sum + x))
done

printf '%.3f\n' "$(echo "scale=4; $sum / $n" | bc -l)"

#
# Remember that printf %.f does rounding. Therefore, use scale=4 (not 3) in bc.
#


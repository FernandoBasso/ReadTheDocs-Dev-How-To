#
# Looping and Skipping
# ====================
#
# https://www.hackerrank.com/challenges/bash-tutorials---looping-and-skipping
#

for num in {1..99} ; do
  if (( $num % 2 )) ; then
    printf '%d\n' $num
  fi
done

#
# - `echo $(( 4 % 2 ))` produces 0.
# - `echo $(( 3 % 2 ))` produces 1.
# - 0 is truthy, 1 is falsy.
#
# https://unix.stackexchange.com/questions/110348/how-do-i-get-the-list-of-exit-codes-and-or-return-codes-and-meaning-for-a-comm
#
# In C/bash and other langs, 0 is truthy because all other positive or negative
# integer can then be used to represent a type of error, something similar to
# HTTP status codes. Try `man ping` and search for `code 1` and `code 2` or
# `man wget` and search for `EXIT STATUS`.
#

#
# We could also use `$(seq 99)` instead of the range. It would be using another
# process and an external program, but it would work.
#

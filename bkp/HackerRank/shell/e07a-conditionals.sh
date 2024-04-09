#!/bin/bash

#
# Getting Started With Conditionals
# =================================
#
# https://www.hackerrank.com/challenges/bash-tutorials---getting-started-with-conditionals/problem
#

read -r answer

case "$answer" in
  [Yy]*)
    echo YES
    ;;
  [Nn]*)
    echo NO
    ;;
  *)
    echo 'Crap 💩'
    ;;
esac


#
# With the [Yn]* syntax, we match either an uppercase or lowercase “y” followed
# by anything.
#
# The match could also be something like this:
#
#      Y*|y*)
#

#!/bin/bash

#
# A Personalized Echo
# ===================
#
# https://www.hackerrank.com/challenges/bash-tutorials---a-personalized-echo
#
#
# Running
# -------
#
#     bash script.sh 'Master Yoda'

printf 'Welcome '
cat

#
# This worked on HackerRank because HackerRank seems to close STDOUT after
# writing to it. On a real command line, it doesn't work like that, but
# it is nice to have it jotted down nonetheless.
#


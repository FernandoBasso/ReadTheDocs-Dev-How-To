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
#     bash script.sh <<<'Master Yoda'
#

xargs -I ^ printf '%s ' Welcome ^
# → Welcome Master Yoda

#
# %s is reused for as many arguments as needed. It leaves a space after
# the last one, though.
#
# Works because HackerRank closes STDOUT after writting to it.
#
# echo could also be used.
#

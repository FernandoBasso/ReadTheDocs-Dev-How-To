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

printf 'Welcome %s\n' "$1"

#
# This version assumes the user is going to pass the name as an argument when
# running the script
#


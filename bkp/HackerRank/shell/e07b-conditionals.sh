#!/bin/bash

#
# Getting Started With Conditionals
# =================================
#
# https://www.hackerrank.com/challenges/bash-tutorials---getting-started-with-conditionals/problem
#
# Running
# -------
#
#     $ bash script.sh
#     y<Enter>
#
# Or
#
#     $ bash script.sh <<<'Nope'
#

read -r answer

[[ "$answer" = [Yy]* ]] && echo SIM
[[ "$answer" = [Nn]* ]] && echo NOPE


#
# Match anything that starts with Y or y, N or n, followed by anything.
#


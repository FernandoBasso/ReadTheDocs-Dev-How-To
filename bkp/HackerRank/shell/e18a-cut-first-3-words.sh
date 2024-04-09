#!/usr/bin/env bash

#
# Cut #8
# ------
#
#  https://www.hackerrank.com/challenges/text-processing-cut-8
#
# Run with:
#
#   bash script.sh 'foo bar baz tux'
#

cut --delimiter=\  --fields=1,2,3 <<<"$1"


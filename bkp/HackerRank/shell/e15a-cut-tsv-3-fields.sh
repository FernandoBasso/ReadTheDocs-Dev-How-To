#!/usr/bin/env bash

#
# Cut #5
# ------
#
# https://www.hackerrank.com/challenges/text-processing-cut-5
#

input=$'Lisp\tScheme\tHaskell\tRacket\tJavaScript'

cut --delimiter=$'\t' --fields=1,2,3 <<<"$input"

#
#   $ bash e15a-cut-tsv-3-fields.sh
#   Lisp    Scheme  Haskell
#


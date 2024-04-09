#!/usr/bin/env bash

#
# Cut #2
# ------
#
# https://www.hackerrank.com/challenges/text-processing-cut-2
#

cut --bytes=2,7 -

#
# man cut says:
#       -b, --bytes=LIST
#           select only these bytes
#
# And by LIST it means a comma-separated list.
#
#
# NOTE
# ----
#
# On HackerRank, --bytes=3,7 produced this error:
#
# cut: unrecognized option: bytes=3,7
#
# BusyBox v1.30.1 (2019-06-12 17:51:55 UTC) multi-call binary.
#
# Usage: cut [OPTIONS] [FILE]...
#
# Print selected fields from each input FILE to stdout
#
# -b LIST Output only bytes from LIST
#
# -c LIST Output only characters from LIST
#
# -d CHAR Use CHAR instead of tab as the field delimiter
#
# -s  Output only the lines containing delimiter
#
# -f N  Print only these fields
#
# -n  Ignored
#


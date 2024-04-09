#!/usr/bin/env bash

#
# Cut #7 - Fourth Word
#
# https://www.hackerrank.com/challenges/text-processing-cut-7
#

cut -d \  -f 4

#
# If a line does not contain the separator (a space in this case), then
# that line is printed (unless the -s option is specified). So, for a line
# that contains something like `foo-bar` (note there is no space anywhere
# in that line), even though there is no fourth field, the text "foo-bar"
# is still displayed. It is documented in the man page.
#
#
#   $ cut -d \  -f 4 \
#       <<<$'hello\nworld\nThe force is strong with this one.'
#   hello
#   world
#   strong
#
#   $ cut -d \  -f 4 -s \
#       <<<$'hello\nworld\nThe force is strong with this one.'
#   strong
#
#
# -f, --fields=LIST
#        select only these fields;  also print any line that contains  no
#        delimiter character, unless the -s option is specified
#

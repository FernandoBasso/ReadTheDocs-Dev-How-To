#!/usr/bin/env ruby -wU

# An array of Fixnum (int).
num_arr = [2, 3, 4]

#
# &:to_s converts each num in turn to a string.
#
str_arr = num_arr.map(&:to_s)


str_arr.each {|n| print "#{n} "}

#
# NOTE that Fixnum class doesn't have .to_sym.
#




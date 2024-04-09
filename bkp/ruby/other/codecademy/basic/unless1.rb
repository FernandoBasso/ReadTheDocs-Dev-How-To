#!/usr/bin/env ruby

#
# unless
# ------
# `unless` is the opposite of `if`.
#
# The `unless` construct allows you to “do something” unless
# a certain condition is true. In other words, do something if
# the condition evaluates to false.
#

puts 'foo' unless false

#
# 2 > 3 is a condition that evaluates to false, therefore, the
# puts statement is executed.
#
unless 2 > 3 then puts '2 is lt 3' end


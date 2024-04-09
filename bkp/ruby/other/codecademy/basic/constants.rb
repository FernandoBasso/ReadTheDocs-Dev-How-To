#!/usr/bin/env ruby

# Define a constant.
MAX_LENGHT = 10

puts MAX_LENGHT


#
# But surprisingly enough, you can change constants in ruby.
# The interpreter will issue a warning, but it'll work nonetheless.
# Avoid changing constants, though.
#
MAX_LENGHT += 1
puts MAX_LENGHT

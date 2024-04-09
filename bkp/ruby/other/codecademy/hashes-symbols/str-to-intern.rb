#!/usr/bin/env ruby -wU

#
# 'string'.intern will “internalize” the string into
# a symbol. `intern` does the same as `to_sym`.
#


#
# An array of strings.
#
strings = [ 'HTML', 'CSS', 'JavaScript', 'Python', 'Ruby' ]

# Create empty array to store symbols.
symbols = []

#
# Convert each string in the string array to a symbol
# and add them to the symbols array.
#
strings.each do |str|
    symbols.push(str.intern)
end

puts symbols

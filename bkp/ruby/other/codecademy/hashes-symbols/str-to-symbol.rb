#!/usr/bin/env ruby -wU

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
    symbols.push(str.to_sym)
end

puts symbols

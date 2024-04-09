#!/usr/bin/env ruby

input = '\0'
words = []

#
# Keeps asking for input until input is ''.
until input.empty?
    input = gets.chomp

    #
    # Turn `input` into a one-element array, and concatenate
    # it into the existing `words` array.
    #
    words += [input] unless input.empty?
    #words += [input] if not input.empty?

    #
    # The above could also be something like:
    #
    #     words.push(input)
    #

end

puts words.inspect

i = 1
words.each do |w|
    puts "Word #{i} is “#{w}”."
    i = i + 1
end


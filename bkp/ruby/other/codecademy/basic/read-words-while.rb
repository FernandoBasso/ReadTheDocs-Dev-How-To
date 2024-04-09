#!/usr/bin/env ruby

#
# Kees reading input until the user hits enter without chars in input.
# For this program, the user should type one word then <Return>. When
# no more words are to be entered, just hit <Return> to input and empty
# value and terminate the program.
#

#
# Storage for input words.
words_if = []
words_unless = []

#
# Dummy text because we need something for the first run.
# That could be avoided with a `do/while`, but that
# construct is not available in ruby.
#
# http://stackoverflow.com/questions/136793/is-there-a-do-while-loop-in-ruby
#
current = 'dummy'

while not current.empty?
    current = gets.chomp

    #
    # If the current word is not empty, add it to the array.
    #
    words_if += [current] if not current.empty?

    #
    # You could also use `unless` instead of `if not`.
    #
    words_unless += [current] unless current.empty?
end

puts "There are #{words_if.length} words in the list. They are:"

if words_unless.length > 0
    words_unless.each do |curword|
        puts curword
    end
end

puts 'End of program.'

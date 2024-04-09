#!/usr/bin/env ruby -wU

#
# In this project, we'll build a program that takes a user's input, then builds a
# hash from that input. Each key in the hash will be a word from the user; each
# value will be the number of times that word occurs.
#

puts 'Text please:'
text = gets.chomp

# `text` comes in as a string. Let us turn it into an array.
words = text.split(' ')

#
# Creates a hash with default value of any key set to zero.
# That means that for every new key that is added, it will
# have a default value of zero.
#
frequencies = Hash.new(0)

words.each do |word|
    # Increments the counter for that word.
    frequencies[word] += 1
end

#
# myhash.sort_by returns an array, and we just override our
# frequencies hash, and assign the returned array to it.
#
# Sorting by the value, from lowest to highest.
#
frequencies = frequencies.sort_by { |a, b| b }

#
# `frequencies` is now an array with sub arrays containing [k, v]
# pairs corresponding to k, v hash pairs.
# [[k1,v1], [k2,v2], ...]
#

#
# You can't reverse a hash. No problem here, though, because
# we turned it into an array after sorting it above.
#
frequencies.reverse! # frequencies.class would say ‘Array’.

#
# Frequencies is actually a 2d array, thus |word, frequency|.
#
frequencies.each do |word, frequency|
    #
    # Can't just concatenate String with Fixnum in ruby.
    # Must explicitly call to_s on ints (Fixnum)
    #
    #puts word + ' ' + frequency.to_s
    puts "#{word} #{frequency.to_s}"
end


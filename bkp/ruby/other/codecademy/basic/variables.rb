#!/usr/bin/env ruby

y = 10

# This works as well.
a, b, c = 11, 22, 33
puts a
puts b
puts c

=begin
#
# BEWARE: This creates an array implicitly.
#
arr = 'yoda', 'vader', 'luke'
puts arr.inspect
=end

if y.kind_of? Integer
    puts 'Integer'
else
    puts 'Not Integer'
end

puts y.class # Fixnum


#
# BEHOLD!
#
# `y.kind_of? Integer` is true, but y.class is Fixnum.
#

puts 'hello'.class # String
puts 3.14.class # Float

z = 'Hello' # z is a string.
z = 13      # now z is an int.

# “toString()” method is simply to_s, no parenthesis required, although you
# can use them if you like.
puts 3.14.to_s

num = 9
puts num.class # Fixnum
str = num.to_s
puts str.class # String

# Convert to binary by specifing to_s(2) (base 2)
puts num.to_s(2) # 1001

puts 254.to_s(2)  # 11111110
puts 254.to_s(8)  # 400.
puts 254.to_s(16) # fe



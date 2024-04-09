#!/usr/bin/env ruby -wU

#
# Blocks are not objects.
#
# A ‘proc’ is a sort of a “saved block”. A proc allows one to
# “save a block” and use it multipe times without having
# to rewrite it several times.
#

#
# Pass a block to Proc.new and store it in a variable and
# you are done creating a proc.
#
multiples_of_3 = Proc.new do |n|
    n % 3 == 0 # returns true or false. <1>
end

#
# Use & when passing the proc. It converts the proc
# to a block.
# `select` will cause `res` to contain only the numbers for which
# the the proc returned true. It is like `filter` in JavaScript.
#
res = (1..100).to_a.select(&multiples_of_3)

res.each {|n| print "#{n} "}

#
# <1> Remember that ruby returns the value of the last expression implicitly.
#


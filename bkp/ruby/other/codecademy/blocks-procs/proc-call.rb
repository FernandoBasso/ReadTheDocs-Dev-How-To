#!/usr/bin/env ruby -wU

the_source = Proc.new do
    puts 'May the source be with you.'
end

#
# It is possible to call a proc by itself, without
# the need to pass it to a block.
#
the_source.call


#
# Passing args to the proc.
#
greet = Proc.new do |msg, name|
    puts msg + ', ' + name
end

greet.call 'Welcome', 'Master Yoda'
# → Welcome, Master Yoda

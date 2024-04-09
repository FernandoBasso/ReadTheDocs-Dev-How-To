#!/usr/bin/env ruby -wU

arr = ['raindrops', :kettles, 'whiskers', :mittens, :packages]

symbol_filter = lambda do |item|
    #
    # Returns true if the item is a symbol, false
    # if it is not.
    #
    item.is_a? Symbol
end


#
# `select` causes the symbols to be returned.
# if you use `collect`, only the trues and falses
# are returned depending whether a item is a symbol (true)
# or not (false).
#
symbols = arr.select(&symbol_filter)
puts symbols

puts arr.collect(&symbol_filter)

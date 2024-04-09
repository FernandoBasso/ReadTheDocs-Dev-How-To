#!/usr/bin/env ruby -w

for i in 1..5
    #
    # Go to the `next` iteration of the
    # loop in case i # is even.
    #
    #next if i % 2 == 0
    next if i.even?
    puts i
end

#
# When a `next` statement runs, remaining statements inside
# a block are skipped, and the code goes immediately to the
# beginning of the block.
#

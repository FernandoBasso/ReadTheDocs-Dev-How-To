#!/usr/bin/env ruby -wU

#
# Checks whether an int is evenly devided
# by 3.
#
def by_three(num)
    return false unless num.is_a? Integer

    return num % 3 == 0
end

puts by_three(6)        # true
puts by_three(9)        # true
puts by_three(11)       # false
puts by_three('dumb')   # false

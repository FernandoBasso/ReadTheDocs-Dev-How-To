#!/usr/bin/env ruby -wKuU

#
# When using `loop do`, you MUST make sure a condition
# will cause the loop to exit by using `break`.
# In the example below, we made sure to keep decreasing num
# by 1, and when num is <= 0 we break out of the loop.
#

num = 20

loop do
    print "#{num} "
    num -= 1
    break if num <= 0
end

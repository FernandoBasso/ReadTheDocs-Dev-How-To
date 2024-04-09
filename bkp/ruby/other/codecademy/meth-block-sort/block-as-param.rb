#!/usr/bin/env ruby -wU

#
# When we do stuff like below, we are actually
# passing a block as parameter to a method.
#
[10, 20, 30, 40].each do |num|
    puts num
end

#
# do/end delimits a block, just as { }:
#
['anakin', 'luke', 'obi-wan', 'yoda'].each {|jedi| puts jedi}

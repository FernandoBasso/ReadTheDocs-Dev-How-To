#!/usr/bin/env ruby -wKU

#
# x..y goes from x up to and including y.
# x...y goes from x up to, but not including y.

for n in 10..20
    puts n
end

#
# Going backwards doesn't work. You have to use
# n.downto(m). There is also n.upto(m).
#
for i in 10.downto(1) do
    puts i
end

#
# This loop simply won't run.
#
for i in 8..1
    puts i
end

#
# https://www.ruby-forum.com/topic/63026
#

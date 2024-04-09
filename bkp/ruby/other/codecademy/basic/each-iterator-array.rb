#!/usr/bin/env ruby -wKuU

arr = [1, 2, 3, 4, 5]

puts "\nUsing { and }:"
arr.each { |cur|
    cur *= 3
    puts "\t#{cur}"
}

#
# It is possible use do/end keywords instead of { }.
#

puts "\nUsing do/end keywords:"
arr.each do |cur|
    cur *= 2
    puts "\t#{cur}"
end

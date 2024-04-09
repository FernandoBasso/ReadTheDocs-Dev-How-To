#!/usr/bin/env ruby -wU

grades = {
    alice: 100,
    bob: 92,
    chris: 95,
    dave: 97
}

#
# `select` allows us to search based on
# a value rather than on a key.
#
lt97 = grades.select do |name, grade|
    grade < 97
end

puts lt97

alice_grades = grades.select do |k, v|
    k == :alice
end

puts alice_grades

#
# You can also use the methods `each_key` and `each_value`.
#

grades.each_key do |k|
    puts k
end

grades.each_value do |v|
    puts v
end

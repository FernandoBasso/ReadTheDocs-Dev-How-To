#!/usr/bin/env ruby -wU

# Create an array of random ints.
nums = [9, 1, 5, 4, 7]

# Print them in the order they were added.
puts 'Random:'
nums.each {|n| puts n}

# Sort the array of ints.
nums.sort!
# Print them again. They should show up from the
# smallest to the greatest.
puts 'Sorted:'
nums.each {|n| puts n}

#!/usr/bin/env ruby -wKuU

nums = [1, 3, 5, 7, 9]

nums.each { |n| puts n }

uuts '--------------------------------'

# Another way to create an array (learned from jQuery code):
arr = '1 3 5 7'.split ' '
arr.each do |item|
  puts item
end

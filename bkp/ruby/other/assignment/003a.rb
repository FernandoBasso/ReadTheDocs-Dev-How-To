
#
# But... for loops do NOT create scope (unlike other blocks).
#
for i in 1..3
  puts i
end
puts "Once more: #{i}"
# →  1
# →  2
# →  3
# →  Once more: 3


#
# As you see, `i` is still accessible outside the `for` block.
#

# The same here. `foo` is defined inside an `if` block but it is available
# after the block as well.
if true
  foo = 'aha!'
end
puts foo
# →  aha!

#
# `times' runs the block N times.
#
3.times do
  puts 'jedi'
end
# → jedi
# → jedi
# → jedi

puts

#
# `times' return the number object used.
#
puts 2.times { puts 'yoda' }
# → yoda
# → yoda
# → 3

#
# NOTE:
# Remember that `puts' and `do/end` blocks (unlike `{ .. }` blockks) cause the
# block to be an argument to `puts' rather than being passed to the method.
#
puts 2.times do
  puts 'leia'
end
# → #<Enumerator:0x000055f00335dad8>
# From the outer `puts', and the block wasn't passed to `2.times'...

#
# Therefore, use parenthesis around the `do/end' block, or `{ ... }`.
#
puts (2.times do
  puts 'leia'
end)
# → leia
# → leia
# → 2 # From the outer `puts'.

#
# `times' can pass an argument to the block.
#
3.times { |n| puts "time #{n}" }
# → time 0
# → time 1
# → time 2


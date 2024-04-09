x = 0
puts x += 1 until x == 3

num = 1
num += 1 until true
puts num
# → 1
# Still one because `true' is already `true', and `num += 1' statement doesn't
# get executed.

# This, however, prints `2' because the test happens only after the block is
# executed at least once.

puts

num = 1
begin
  puts num
  num += 1
end until true
puts num
# → 2

puts

y = 0
puts y += 1 while y < 3
# → 1
# → 2
# → 3

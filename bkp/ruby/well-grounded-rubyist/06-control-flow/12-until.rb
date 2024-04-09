#
# Until is like `while'. It iterates until a condition is becomes true.  That
# is, while the condition is false, it keeps going. When the condition becomes
# true, it stops.
#

num = 4
until num > 4
  puts "num: #{num}"
  num += 1
end

puts

cnt = 4
begin
  puts "cnt: #{cnt}"
  cnt += 1
end until num > 4
# Will execute at least once because the condition is tested only after
# the block is entered at least once.


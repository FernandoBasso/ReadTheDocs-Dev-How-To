num = 1
while num <= 5
  puts "Num: #{num}"
  num += 1
end

# We print a newline here.
puts

cnt = 1
begin
  puts "cnt: #{cnt}"
  cnt += 1
end while cnt <= 5

#
# `begin/end while' in Ruby is similar to `do/while <condition>' in C and
# some other languages. With `while', it may happen that the loop isn't
# enterend even once. With `do/begin', it always runs at least once because
# the testing condition is done only after the first iteration.
#

puts
puts 'Difference between `while\' vs `begin/end\'.'

x = 5
while x < 5
  # This won't execute. The condition is false from the start.
  puts "x: #{x}"
  x += 1
end

y = 5
begin
  # Will be printed once.
  puts "y: #{y}"
  y += 1
end while y < 5
# By the time we test the condition, the iteration has already happened
# at least one time.


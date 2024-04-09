
# loop { puts 'Looping forever and ever until the end of time...' }

num = 1
loop do
  puts "Iteration #{num}"
  break if num == 9
  num += 1
end

counter = 1
loop do
  puts "Counter is #{counter}"
  counter += 1
  next unless counter == 10
  break
end


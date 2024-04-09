
num = -3

while num < 5
  puts num
  num += 2
end

arr = ['foo', 'bar', 'jedi']
i = 0

while arr[i]
  puts arr[i]
  i += 1
end


i = 0
puts arr[i += 1] while arr[i]

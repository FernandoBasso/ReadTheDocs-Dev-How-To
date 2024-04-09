x, y = 50, 100

puts x if x < 100

def show(num)
  return unless num > 100
  p "Num is > 100: #{num}"
end

show(100)
show(101)



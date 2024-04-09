

class Array
  def our_each
    index = 0
    # Cache size so it doesn't have to invoke self.size each time.
    size = self.size
    until index == size
      # Passes the current array index element to the block.
      yield(self[index])
      index += 1
    end
    # Just to return the array itself.
    self
  end
end

p ([1, 2, 3].each do |num|
  puts num * num
end)
# → 1
# → 4
# → 9
# [1, 2, 3]

puts

class Array
  def other_each
    len = self.size
    len.times do |index|
      yield(self[index])
    end
    self
  end
end

p [-1, -2].other_each { |num| p num * 2 }
# → -2
# → -4
# [-1, -2]


#
# Implement times in terms of `our_each'.
#
class Integer
  def my_times
    p self
    (0..self).to_a.our_each { |n| yield(n) }
    self
  end
end

2.my_times { |num| p num }



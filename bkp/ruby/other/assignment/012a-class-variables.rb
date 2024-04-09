
class Foo

  # class variable
  @@number = 0

  # getter
  def get_it
    @@number
  end

  # setter
  def update_it
    # increment by 1
    @@number -= -1
  end

end


class Bar < Foo

end

foo = Foo.new
bar = Bar.new

puts foo.get_it
puts bar.get_it
# →  0
# →  0

# Changing bar makes it visible to foo as well.
bar.update_it

puts foo.get_it
# →  1

3.times do
  foo.update_it
end

p bar.get_it
# →  4


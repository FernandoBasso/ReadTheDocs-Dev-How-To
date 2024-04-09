#
# `loop' is an iterator. An iterator is a Ruby method that has an extra
# ingredient in its calling syntax: it expects you to provide it with a code
# block
#

def my_loop
  yield while true
end

num = 1
my_loop do
  puts num
  num += 1
  break if num == 4
end

#
# Yielding takes place while the method is still running. After the code block
# executes, control returns to the method at the statement immediately following
# the call to yield .
#
# The block a method takes is part of the method call; it is _not_ an argument
# to the method.
#


#
# `times' and `each` return the receiver. `map' has to return something
# to the method that yielded to it.
#
# Just as the method can yield a value, so too can the block return a value.
# The return value comes back as the value of the call to `yield'.
#

class Array
  def our_map
    cnt = 0
    acc = []
    # `size' is called on `self', the default receiver.
    until cnt == size
      acc << yield(self[cnt])
      cnt += 1
    end
    acc
  end
end

res = ['x', 'y', 'z'].our_map { |e| e.upcase }
p res

#
# Define `my_map' in terms of `each'.
#
class Array
  def my_map
    acc = []
    each { |e| acc << yield(e) }
    acc
  end
end

p ['X', 'Y', 'Z'].my_map { |elem| "#{elem}s" }


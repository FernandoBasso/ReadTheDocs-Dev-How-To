#
# Monkey patch the Array class.
#

class Array
  def random_each(&b)
    #
    # What is `b`?
    #
    p b # <1>
    # → #<Proc:0x00000002c0a268@04-arr-rand.rb:18>

    shuffle.each do |el|
      b.call el
    end
  end
end

[10, 20, 30, 40, 50].random_each do |item|
  p item
end


#
# As seen in <1>, a Proc object is essentially a “nameless” or “annonymous” function
# or block of code that can be represented as an object and can be passed around
# and called at will.
#
# Still, you can only pass a single block to a method at a time. But you can pass
# multiple procs around (procs -are- regular objects).
#

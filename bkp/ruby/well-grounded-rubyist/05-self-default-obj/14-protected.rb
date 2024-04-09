class C
  def initialize(n)
    @n = n
  end

  def n
    @n
  end

  def compare(c)
    if c.n > n
      puts 'The other object is bigger.'
    else
      puts 'The other object is the same or smaller.'
    end
  end
end

C.new(7).compare(C.new(8))
# → The other object is bigger.


#
# Private methods can be called when `self' is an instance of some
# class in the hierarchy.
#
# `n' is private, but we can ask `c' to call it because both
# `self' and `c' are instances of the same class. We could also
# call `n' on `c' if both `c' and `n' belonged to the same class
# hierarchy.
#


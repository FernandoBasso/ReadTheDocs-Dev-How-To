# coding: utf-8
# <FernandoBasso> I have a class instance variable like @foo = 0. How do I do something to it every time I create an instance of that class?
# <FernandoBasso> I tried def initialize; self.foo = <some value>; end but I get an error.
# <apeiros> FernandoBasso: create a class method which does what you want, then self.class.that_method
# <apeiros> (self is the instance, not the class, so self.foo can't possibly work on the class)
# <FernandoBasso> OK. At least I considered that would be an option.
# <FernandoBasso> Ah, good hint!
# <apeiros> +within initialize (or any instance method)
# <FernandoBasso> Thanks.
# <apeiros> only within the class body, self references the class.
# <apeiros> note: class Thing; class << self; attr_reader :count; end; end # <- that'd be the syntax to use attr_reader for class instance variables.

#
# "Create a class method which does what you want, then `self.class.that_method`
# withing `initialize` or any instance method.
#

class Thing

  # class instance variable
  @count = 0

  def initialize
    # invokes the class method which increments
    # the class instance variable count.
    self.class.increment
  end

  # class method which increments the class instance variable.
  def self.increment
    @count -= -1
  end

  # class method as a getter for the class instance variable.
  def self.count
    @count
  end

end

3.times do
  Thing.new
end

p Thing.count
# → 3

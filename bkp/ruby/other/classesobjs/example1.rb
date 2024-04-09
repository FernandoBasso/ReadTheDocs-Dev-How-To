
class Jedi

  @@class_variable = 'from a class var'

  @class_instance_variable = 'from an class instance var'

  def initialize
    @instance_variable = 'iv str iv'
  end


  #
  # Using `self` allows we to call `Jedi.foo` (instead of callling `foo` from
  # an instance of Jedi. This method belongs to the class, not to an instance
  # of it. You can't call `foo` from an instance of `Jedi`, only from `Jedi` itself.
  #
  def self.foo
    puts @@class_variable
    puts @class_instance_variable
    puts @instance_variable       # Oops! Run ruby with -w.
  end

  #
  # This method can only be called from an instance of `Jedi`.
  #
  def foo
    puts @instance_variable
  end
end

#
# Calls the method on the class.
#
Jedi.foo

#
# Calls another method (although with the same name) on an instance
# of the class.
#
j = Jedi.new
puts j.foo



#
# A method `self.foo` can be called from the ClassName iteself. Such method
# can either access class variables or class instance variables, but not
# instance variables (those can only be accessed from methods not defined
# with `self`).
#


class Dog
  attr_reader :age, :dog_years

  def dog_years=(years)
    @dot_years = years
  end

  def age=(years)
    @age = years
    # Would consider dog_years an lvar.
    #dog_years = years * 7
    # So, we use self, and self inside the this instance method
    # refers to the instance object.
    self.dog_years = years * 7
  end

  private :dog_years=
end

#
# The way Ruby implements private methodos is by not allowing an explicit
# receiver. But then, it would be impossible to have a private setter
# method, since doing
#
#   something = 'foo'
#
# and `something' is a setter method, would actually be understood
# as a local variable.
#
# The way Ruby allows private writer methods is by allowing the receiver
# `self' (and it has to be precisely the keyword `self', not a reference
# to it) to be used.
#
#   self.something = 'foo'
#


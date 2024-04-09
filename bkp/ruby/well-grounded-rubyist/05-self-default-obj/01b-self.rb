
###############################################################################
# CIVAR vs IVAR
#

class MyClass

  #### CIVAR ####
  #
  # This is an instance variable, and it is bound to self. Inside the toplevel
  # of the class, self is the class itself. So, @an_ivar is an instance variable
  # of the class MyClass. Classes are objects, that is why classes have instance
  # variables. Some also call these as "class instance variables", or CIVARs.
  #
  @an_ivar = 'a civar' # Could also name it as @an_civar (class instance variable).

  #### IVAR ####
  def initialize
    # This is an instance variable too, and it is also bound to self. Inside
    # this instance method, self is the object created with MyClass.new.
    # This ivar is a different than the one defined above.
    @an_ivar = 'an ivar'
  end

  # A reader accessor for the class instance variable.
  def self.ivar
    @an_ivar
  end

  # A reader accessor for the instance variable.
  def ivar
    @an_ivar
  end
end

p MyClass.ivar
p MyClass.new.ivar
# → "a civar"
# → "an ivar"

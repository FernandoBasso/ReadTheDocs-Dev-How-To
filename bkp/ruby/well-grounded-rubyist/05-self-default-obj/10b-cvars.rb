#
# A class with a cvar and a civar (but not an ivar).
#
# cvars are visible from:
#   - the toplevel of the class
#   - inside class methods
#   - inside instance methods
#
# In short:
#
# Class methods can access:
#   - class variables
#   - class instance variables
#
# Instance methods can access:
#   - class variables
#   - instance variables
#

class Foo
  @@z = 'a cvar'
  @z = 'a civar'

  # A class method (which is a method on the object Foo in this
  # example). Inside a class method, both cvars and civars can
  # be accessed.
  def self.print_vars
    p @@z
    p @z
  end

  # An instance method. It can access cvars and ivars, but not civars. Note
  # that inside this instance method @z is not the same as the previous @z
  # defined at the toplevel of the class.
  def print_vars
    p @@z
    p @z # <1>
  end
end

Foo.print_vars
Foo.new.print_vars

#
# <1>. Here, inside an instance method, `@z' is an instance variable. As in
# this example class we _don't_ have an ivar (we have a cvar and a civar), when
# we try to `p @z', which is an ivar at this location, we get `nil'.
#





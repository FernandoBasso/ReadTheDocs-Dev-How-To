#
# BEWARE: Ruby class CLASS VARIABLES.
#
# BEHOLD: Ruby has CLASS INSTANCE VARIABLES as well.
#
# DESPAIR: It is easy to get confused.
#
# HAVE FAITH IN THE FORCE: it is not so hard to understand them.
#
# Attr accessors don't work for class variables. We can't do
#
#   class << self
#     attr_reader :my_cvar
#   end
#
# Attr accessors can only be used for civars and ivars. That means this doesn't
# work:
#
#   attr_accessor :foo
#
# It will either create an accessors for @foo ivar, or @foo civar, but not for
# @@foo. If you _really_ need accessors for cvars, define them explicitly,
# verbosely, like showed below.
#
# The de-facto example for class variables is for one that counts the number
# of instances created from the class in question.
#

class Example
  # This is a cvar (not a civar).
  @@count = 0

  # The only way to define a reader accessor for @@count cvar is using
  # an explicit method like this.
  def self.get_count
    @@count
  end

  # or using this syntax
  class << self
    def read_count
      @@count
    end

    # a writter accessor for @@count
    def set_count=(num)
      @@count =  num
    end
  end

  def initialize
    # Each time we create a new instance of Example, increment
    # @@count by one.
    @@count += 1
  end
end

3.times do
  Example.new
end
p Example.get_count
Example.set_count = 1984
p Example.read_count
# →  3
# →  1984

#
# A CIVAR is an IVAR, but a _special_ case of IVAR. While an IVAR belongs
# is bound to an object created by instantiating a class, a CIVAR is bound
# to the class object itself.
#


#
# You can't define an instance variable and try to access it directly. You always
# need getters/setters because instance variables are private.
#
# Also, you if you use attr_reader, attr_writer or `attr_accessor` you
# automatically create a getter/setter for one or more instance variables.
#

class Person

  # `initialize` is the constructor.
  def initialize
    @name = 'unknown'
  end

  # Setter.
  def name= name
    @name = name
  end

  # Getter.
  def name
    @name
  end

end

# Instantiate `Person`.
p = Person.new

# Uses the getter.
puts p.name

# Uses the setter.
p.name = 'Yoda'

# Uses the getter again.
puts p.name


class Greeter

    #
    # Causes `name` to become a getter, and `name=` to become a setter.
    # Basically, makes @name public, you just don't access it with
    # obj.@name, but simply obj.name instead.
    #
    attr_accessor :name

    #
    # Note that we provide a default value in case we don't
    # want or need to pass a name when creating an instance
    # of the Greeter class.
    #
    def initialize(name = 'World')
        @name = name
    end

    def say_hi
        puts "Hi, #{@name}!"
    end

    def say_bye
        puts "Bye, #{@name}. Come back son, please."
    end
end

#
# Not passing the argument to the constructor causes it
# to use the default value that we provided when defining
# the class and its initialize method.
#
obj = Greeter.new
puts obj.name

# Assign the initial name through the constructor.
greeter = Greeter.new('Yoda')
puts greeter.name   # Uses the getter.

# Uses the setter.
greeter.name = 'Dart Vader'
puts greeter.name

#
# BEWARE!
# -------
# obj.prop does NOT mean prop is public. Just that in ruby we don't
# use getProp and setProp. We just use prop, and ruby knows when
# it is a getter or a setter depending on the use of the = operator.
#

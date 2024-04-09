#
# A class!
class Greeter

    #
    # `initialize` is the constructor method.
    #
    def initialize(name = 'World')
        #
        # @name is an instance variable.
        #
        @name = name
    end

    #
    # Some other methods.
    #

    def say_hi
        puts "Hi, #{@name}!"
    end

    def say_bye
        puts "Bye, #{@name}. Come back son, please."
    end
end

#
# Create an instance of the class, passing an argument
# to the constructor.
#
greeter = Greeter.new('Yoda')
#
# Call the methods.
#
greeter.say_hi
greeter.say_bye

#
# Remember, when methods don't take parameters, you may
# omit the parenthesis.
#


# See all defined and inherited stuff.
puts Greeter.instance_methods # Or ()

# See only explicitly defined stuff.
puts Greeter.instance_methods(false)

greeter.respond_to?('name')    # false
greeter.respond_to?('say_hi')  # true
greeter.respond_to?('say_bye') # true

greeter.name # Error. Ruby thinks name is a method, but it is
             # actually a private property.



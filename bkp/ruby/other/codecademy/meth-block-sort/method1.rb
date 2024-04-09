#!/usr/bin/env ruby -wU

#
# Let's define a simple method (function).
#
def greeting
    puts 'Greetings, honorable gentleman.'
end

#
# Call the method (make it run).
#
greeting

#
# NOTE that in Ruby it is not necessary to add parenthesis
# after method names to call the method. This is especially
# true when the method takes no arguments, like the one
# we just defined.
#

#
# We can use parenthesis if we want, altough Ruby folks
# generally don't use them when they are not really necessary.
#
greeting()

#
# Calling a method that doen't exist causes a NameError.
#

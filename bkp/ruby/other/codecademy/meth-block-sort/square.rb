#!/usr/bin/env ruby -wU

#
# Squres numbers if they are integers.
# Stops in case input is string or float.
#
def square(param)

    #
    # Quit if not an int.
    #
    if not param.is_a? Integer
        puts "‘#{param}’ is not an integer. Bailing out..."
        return false
    end

    #
    # Square and print it.
    #
    puts "#{param} squared is #{param ** 2}."

end

#
# Call with different argument types.
#
square(5)

square('dummy')

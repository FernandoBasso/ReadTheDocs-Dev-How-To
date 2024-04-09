#!/usr/bin/env ruby -wU

def prime(n)
    # Using `unless`.
    puts 'That\'s not an integer.' unless n.is_a? Integer

    #
    # Stop the program in case n is not an int.
    #
    return false unless n.is_a? Integer

    #
    # Otherwise, let us go on.
    #

    # Using `if not`.
    #puts 'That\'s not an integer.' if not n.is_a? Integer

    is_prime = true

    for i in 2..n - 1
        if n % i == 0
            is_prime = false
        end
    end

    puts is_prime ? "#{n} is prime." : "#{n} is not prime."

end

prime('n')
prime(5)

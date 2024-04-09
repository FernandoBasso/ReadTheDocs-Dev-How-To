#!/usr/bin/env ruby -wU

#
# Allows the use of the old-style Prime.new.
#
VERBOSE = nil

require 'prime'

def first_n_primes(n)
    unless n.is_a? Integer
        return 'n must be an integer.'
    end

    if n <= 0
        return 'n must be greater than zero.'
    end

    prime_array = [] if prime_array.nil?

    prime = Prime.new
    for num in (1..n)
        prime_array.push(prime.next)
    end

    return prime_array
end

puts first_n_primes(10)

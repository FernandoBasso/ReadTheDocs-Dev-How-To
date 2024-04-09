#!/usr/bin/env ruby -wU

require 'prime'

def first_n_primes(n)

    return 'n must be an integer.' unless n.is_a? Integer

    return 'n must be greater than 0.' if n <= 0

    prime = Prime.instance # ruby >= 1.9.
    prime.first n
end

puts first_n_primes(10)

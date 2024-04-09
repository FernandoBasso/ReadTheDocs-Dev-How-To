#!/usr/bin/env ruby -wU

VERBOSE = nil

require 'prime'

def first_n_primes(n)

    return 'n must be an integer.' unless n.is_a? Integer

    return 'n must be greater than 0.' if n <= 0

    prime_array = [] if prime_array.nil?
    prime_array ||= [] # Conditional assignment.

    prime = Prime.new
    n.times do
        prime_array << prime.next
    end

    prime_array
end

puts first_n_primes(10)

require_relative 'is_prime_v1'
require_relative 'is_palindrome_v1'

##
# Generates the first `len` palindromic primes.
#
def prime_palind(len)
  1.upto(Float::INFINITY).lazy.select do |n|
    prime?(n) && palind?(n.to_s)
  end.first(len)
end

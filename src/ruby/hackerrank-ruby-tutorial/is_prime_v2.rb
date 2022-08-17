# tags: [prime, math, algorithm]

##
# Checks whether `n` is prime.
#
# 0 and 1 are not primes.
#
# References:
#
# • https://en.wikipedia.org/wiki/Prime_number
# • https://math.stackexchange.com/questions/5277/determine-whether-a-number-is-prime
# • https://www.geeksforgeeks.org/c-program-to-check-whether-a-number-is-prime-or-not/
#
def prime?(n)
  return false if n < 2

  (2..n).each do |d|
    return false if (n % d).zero?
  end

  true
end

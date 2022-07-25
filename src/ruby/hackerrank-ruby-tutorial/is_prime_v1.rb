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
  return false if n.zero?
  return false if n == 1

  lim = Math.sqrt(n)
  is_prime = true
  i = 2

  while i <= lim
    if (n % i).zero?
      is_prime = false
      break
    end

    i += 1
  end

  is_prime
end

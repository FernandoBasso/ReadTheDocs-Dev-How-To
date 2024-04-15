##
# Finds the indices of the two prices that sum to `money`.
#
# • T.C: O(n).
#
# ASSUME:
#
# • The input always contains at least two distinct values.
# • The input always contains a single, valid solution.
#
# @param money [Integer]
# @param prices [Array<Integer>]
# @return [Array<Integer>]
#
def parlor(money, prices)
  ##
  # `seen` is used to store at which index the complement
  # as been seen before. The key is the complement itself,
  # and the value is the index.
  #
  seen = {}
  idx = 0

  # for price in prices do
  prices.each do |price|
    complement = money - price

    return [seen[complement], idx + 1] if seen[complement]

    seen[price] = idx + 1
    idx += 1
  end
end

##
# Finds the indices of the two prices that sum to `money`.
#
# • T.C: O(n²)
#
# @param money [Int]
# @param flavorPrices [Array<Int>]
# @return [Array<Int>]
#
def parlor(money, flavorPrices)
  p flavorPrices
  lastIdx = flavorPrices.size - 1
  indices = [-1, -1]

  for i in 0..lastIdx do
    for j in (i + 1)..lastIdx do
      if flavorPrices[i] + flavorPrices[j] == money then
        indices[0] = i + 1
        indices[1] = j + 1
      end
    end
  end

  indices
end

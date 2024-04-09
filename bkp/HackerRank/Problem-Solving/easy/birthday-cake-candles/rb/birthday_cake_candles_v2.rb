##
# Finds how many times the largest number is found in `candles`.
#
# @sig [Int] -> Int
#
# @param candles [Array] The array of integers.
# @return [Number] The number of times the largest number is found.
#
def birthday_cake_candles(candles)
  candles.count(candles.max)
end

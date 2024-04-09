
##
# @param x Integer
# @param y Integer
# @return Integer
#
def get_sum(x, y)
  #
  # Find min and max. We need them to calculate the number
  # of elements in the sequence.
  #
  min = [x, y].min
  max = [x, y].max

  #
  # Calculate the number of elements in the sequence.
  # `n` is needed for the actual formula.
  #
  n = max - min + 1;

  #
  # Apply the formula.
  #
  n * (max + min) / 2
end

p get_sum(1, 5)
# → 15

p get_sum(-2, 4)
# → 7

class Integer
  ##
  # Checks whether the receiver is between `x`
  # and `y`, inclusive.
  #
  def range?(x, y)
    self >= x && self <= y
  end
end

#
# Is 1 between 0 and 3‽
#
p 1.range?(0, 3)
# → true

x, y = 5, 6

# TODO: this is not working.

puts case
  when x > y
    x + y
  when x < y
    x - y
  when x == y
    x
  else
    0
  end

# → -1

#
# If nothing matches, not even the `else' (which is not the case for this
# example), then `nil' is returned. In this example, `-1' is returned, and
# therefore printed by `puts'.
#


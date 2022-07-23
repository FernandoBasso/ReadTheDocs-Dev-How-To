require 'awesome_print'

add = ->(x, y) { x + y }

ap add.call(-1, 1)
#
# → 0
##

##
# Partially apply `add` to 1.
#
add1 = add.curry.call(2)

ap add1.call(10)
#
# → 11
##

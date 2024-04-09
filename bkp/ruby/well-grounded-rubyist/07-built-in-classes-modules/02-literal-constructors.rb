
#
# Most of Ruby built-in classes can be instantiated using `new'. Just a few
# can't.
#
#   str = String.new('foo')
#   arr = Array.new(3) # length 3 (like in js), not an array with the element 3
#
# But this is an error:
#
#   Integer.new
#   Integer.new(1)
#
# A lucky, select few classes have _literal constructors_, which means you can
# use a special syntax to create objects.
#

str = 'my string'
sym1 = :my_symbol
sym2 = :'my symbol with special! chars‽'
arr = [:foo, 'bar', 2.5]
hash1 = { id => 9, name => 'Yoda' }
hash2 = { id: 9, name: 'Yoda' }

# ranges
p 1..2
p 1...3

re = /[a-z]+/i

# Proc and lambdas.
# Dash, arrows, parentheses, braces
# -> (x, y) { x * y }

#
# `[]` is not always a literal constructor. { } do not always denote a hash
# either. Overloading of notation is a consequence of a finite number of chars
# on any given keyboard. Pay attention to context.
#

# + is a method, but it is sugared.
p 1 + 2 == 1.+(2)
# → true


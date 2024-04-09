#
# Each local scope as its own supply of lvars. Two foo variables in two
# different local scopes have nothing to do with one another.
#
# The toplevel is its own local scope. Every class, module, method, block, have
# their own local scope.
#

if true then
  lorem = 'lorem'
end
p lorem
# →  "lorem"
# As we see, if blocks are not an isolated scope from the outer scope.

for i in 1..1
  ipsum = "#{i} ipsum"
end
p ipsum
# "1 ipsum"
# for, while, loop, and similar iterators also share scope with the outer scope.

1.times do
  # It can access variables from the outer scope.
  p ipsum # Works!

  # But variables created inside the block cannot be accessed outside.
  dolor = 'dolor'
end
#p dolor
# Error: undefined local variable or method `dolor'
# But blocks do have limitted scope.

def a_method
  p lorem # Error, can't access it here.
  amet = 'amet'
end
a_method() # Error because of `p lorem' inside `a_method'.
p amet # Error: undefined local variable or method `amet'.


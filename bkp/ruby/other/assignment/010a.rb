
puts foo if foo = 0.zero?

#
# Rather than printing “true” you receive a NameError, “undefined local variable
# or method `a’”. Since ruby parses the bare a left of the if first and has not
# yet seen an assignment to a it assumes you wish to call a method. Ruby then
# sees the assignment to a and will assume you are referencing a local method.
#
# The confusion comes from the out-of-order execution of the expression. First
# the local variable is assigned-to then you attempt to call a nonexistent
# method.
#

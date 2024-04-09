
# Does not assign to `bar`. Still, it tells the interpreter that `bar` is a
# variable (which gets assigned nil). The local variable is created when the
# parser encounters the assignment, not when the assignment occurs.
bar = 3.14 if false
puts bar
# →  nil or an empty line.

jedi = 'the force' if true
puts jedi
# →  'the force'


puts flag if flag = 0.zero?
# →  Error... aborts execution.

# Because of the above error, we don't even actually get to this point.
puts foo    # →  error about no method foo. Aborts execution.

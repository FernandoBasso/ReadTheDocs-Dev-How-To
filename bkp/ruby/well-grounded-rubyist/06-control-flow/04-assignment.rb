# This condition will _always_ succeed.
if num = 10
  puts 'yes'
end
# → yes
# “warning: found = in conditional, should be ==”

val = nil

# Will be falsy.
if thing = val
  puts 'what‽' # This won't be printed.
end
# But no warning, because Ruby doesn't know for sure whether assigning
# `val' to `thing' will be truthy or falsy.

if false
  # The creating of the variable happens, but not the assignment to it.
  foo = 'hello'
end

p foo # → nil
p bar # → fatal error
# “undefined local variable or method `bar' for main:Object (NameError)”


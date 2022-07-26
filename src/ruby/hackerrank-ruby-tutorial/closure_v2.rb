def add1(f)
  f.call
end

#
# We intentionlly do NOT define `x` before creating the lambda/closure.
#
# x = 1
#

fn = -> { x + 1 }

x = 10
# Useless assignment to variable - `x`

p add1(fn)
#
# undefined local variable or method `x'
#
# Because no `x` was defined before we defined the lambda/closure,
# we get an error saying `x` does not exist.
#

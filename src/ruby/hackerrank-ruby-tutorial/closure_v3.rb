def add1(f)
  f.call
end

x = 1

fn = -> { x + 1 }

x = 10

p add1(fn)

#
# Which `x` will the closure remember?
#
# x = 10 will be remembered. The output is 11.
#
# So, if `x` is defined earlier, and then reassigned, it remembers
# its last value. But as we saw in the previous example, if it is
# only defined after we declare the closure, then it throws an error.
#
# Tricky stuff to remember and avoid.
#

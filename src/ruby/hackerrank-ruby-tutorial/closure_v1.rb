def add1(f)
  f.call
end

##
# `x` is in the top level scope. Yet, `fn` can remember its value
# when called inside `add1`.
#
# When we define `fn`, it references `x`. `fn` will remember the
# value of `x`
x = 1

fn = -> { x + 1 }

p add1(fn)

##
# A function-like object that takes one numeric parameter and
# increments it by 1.
#
add1 = proc { |n| n + 1 }

##
# Takes an `x` value and a proc and call the proc with the value.
#
def f(x, a_proc)
  a_proc.call(x)
end

p f(0, add1)

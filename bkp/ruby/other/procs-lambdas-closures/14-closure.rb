
#
# Using a closure to dynamically define a function behavior.
#

# <1>
def multiple_generator m
    # <2>
    lambda do |n|
        n * m
    end
end

doubler = multiple_generator 2 # <1a>
tripler = multiple_generator 3 # <1b>

p doubler.call(5)
# → 10
p tripler.call(5)
# → 15

#
# <1>. 2 <1a> and 3 <1b> are both stored in `m` in <1>. Later, when invoking
# `doubler` and `tripler`, the value of `m` is remembered inside the closure.
#
# <2>: # Remember that ruby returns its last expression. In this case, it happens
# to be a lambda. Therefore, `doubler` and `tripler` end up referencing a lambda
# that has predefined `m` values of 2 and 3.
#




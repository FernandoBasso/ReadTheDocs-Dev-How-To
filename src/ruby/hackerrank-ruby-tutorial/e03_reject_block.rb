div_by_3 = Proc.new do |n|
  n % 3 == 0
end

p (1..9).to_a.reject(&div_by_3)
#
# → [1, 2, 4, 5, 7, 8]
##

div_by_4 = Proc.new { |n| n % 4 == 0 }

p (1..9).to_a.select(&div_by_4)
#
# → [4, 8]
##

is_negative = lambda { |n| n < 0 }

p (-3..3).to_a.reject(&is_negative)
#
# → [0, 1, 2, 3]
##

is_positive = ->(n) { n > 0 }
p (-3..3).to_a.select(&is_positive)
#
# → [1, 2, 3]
##

#
# NOTE: We can't define a lambda in the outer scope
# and try to use it inside a method. This wouldn't work:
#
#   f = lambda { |n| n > 0 }
#   def g(xs)
#     xs.reject(&f)
#   end
#
# undefined local variable or method `f`.
#
# But this would work.
#
f = lambda { |x| x >= 0 }
def g(xs, lmbd)
  xs.select(&lmbd)
end

p g((-3..3).to_a, f)

require "rspec/autorun"

class Calc
  class << self
    def add(x, y)
      x + y
    end

    #
    # 5! → 5 * 4 * 3 * 2 * 1 → 120
    #
    def fact(n, acc = 1)
      return acc if n <= 1
      # This is internally optimized and does not cause any stack overflow.
      #(1..n).reduce(&:*)
      fact(n - 1, acc * n)
    end
  end
end

describe Calc do
describe '#add' do
  it 'adds two numbers' do
    expect(Calc.add(1, 1)).to eq(1 + 1)
  end

  it 'adds two different numbers' do
    expect(Calc.add(-2, -3)).to eq(-2 + -3)
  end
end

describe '#fact' do
  it 'returns 1 when given 0' do
    expect(Calc.fact(0)).to eq(1)
  end

  it 'returns 120 when given 5' do
    expect(Calc.fact(5)).to eq(120)
  end

  it 'works with very large inputs' do
    expect(Calc.fact(512 * 8)).to be > 1024
  end

end
end

########################################################################
# NOTES
# -----
#
# This solution would overflow the stack call with large inputs:
#
#   def fact(n)
#     return 1 if n <= 1
#     n * fact(n - 1)
#   end
#
# Some Ruby implementations do not support TCO (Tail Call Optimization)
# while others do. See:
#
# This solution will work in some implementations. We are (trying to,
# since it depends on the Ruby implementation) use tail recursion. Note
# the ‘acc’ param. That is how we would do it in Scheme, for instance.
#
#   def fact(n, acc = 1)
#     return acc if n <= 1
#     fact(n - 1, acc * n)
#   end
#
# Info about TCO in Ruby:
# https://harfangk.github.io/2017/01/01/how-to-enable-tail-call-recursion-in-ruby.html
#
#
# In Chicken Scheme it would be something like this (would produce an
# astoundingly large number as result, and it would work just fine):
#
#   ;;
#   ;; Calculates the factorial using tail recursion.
#   ;;
#   ;; Usage:
#   ;;
#   ;;   (fact 120)
#   ;;   (fact (* 512 32))
#   ;;
#   (define (fact n)
#     (let go ((x n) (acc 1))
#       (cond
#        ((< x 1) acc)
#        (else
#         (go (- x 1) (* acc x))))))
#

#
# vim: set nowrap:
#

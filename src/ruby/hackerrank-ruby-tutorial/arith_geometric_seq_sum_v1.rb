#
# tags: [inject, reduce, enumerator]
#

##
# Sum the first `n` terms of an arithmetico-geometric sequence.
#
def sum_terms(n)
  (1..n).inject(0) { |acc, x| acc + x ** 2 + 1 }
end

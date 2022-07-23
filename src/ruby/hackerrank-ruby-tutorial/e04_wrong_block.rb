#
# p (-3..3).to_a.reject({ |n| n < 0 })
# syntax error, unexpected '|', expecting '}'
# possibly useless use of < in void context
# syntax error, unexpected '}', expecting end-of-input
#

p (-3..3).to_a.reject { |n| n < 0 }
#
# → [0, 1, 2, 3]
##

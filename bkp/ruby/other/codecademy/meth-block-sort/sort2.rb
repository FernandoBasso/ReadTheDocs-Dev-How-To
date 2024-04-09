#!/usr/bin/env ruby -wU

#
# Combined Comparison Operator: <=>
#  0 if a == b
#  1 if a > b
# -1 if a < b

books = [
  "Charlie and the Chocolate Factory",
  "War and Peace", "Utopia",
  "A Brief History of Time",
  "A Wrinkle in Time"
]

#
# Sort in descendent order.
#
books.sort! do |book_a, book_b|
  book_b <=> book_a
end

puts books

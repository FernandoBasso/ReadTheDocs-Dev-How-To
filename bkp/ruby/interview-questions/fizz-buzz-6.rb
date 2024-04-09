#
# https://www.tomdalling.com/blog/software-design/fizzbuzz-in-too-much-detail/
#

def fizzbuzz(range, triggers)
  range.map do |num|
    parts = triggers.select { |_, predicate| predicate.call(num) } # <1>
    # puts parts
    parts.size > 0 ? parts.map(&:first).join : num
  end
end

puts fizzbuzz(1..25, [
  ['Fizz', -> (n){ n % 3 == 0 }],
  ['Buzz', -> (n){ n % 5 == 0 }],
  ['Zazz', -> (n){ n < 10 }],
])

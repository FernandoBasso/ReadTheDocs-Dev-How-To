#
# https://www.tomdalling.com/blog/software-design/fizzbuzz-in-too-much-detail/
#

def fizzbuzz(range, triggers)
  range.each do |num|
    result = ''
    triggers.each do |(text, predicate)|
      result << text if predicate.call(num)
    end
    puts result == '' ? num : result
  end
end

#
# Passing anonyous function predicates to test the trigger.
#
fizzbuzz(1..25, [
  ['Fizz', -> (n){ n % 3 == 0 }],
  ['Buzz', -> (n){ n % 5 == 0 }],
  ['Zazz', -> (n){ n < 10 }],
])


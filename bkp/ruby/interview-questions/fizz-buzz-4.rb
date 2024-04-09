#
# https://www.tomdalling.com/blog/software-design/fizzbuzz-in-too-much-detail/
#

def fizzbuzz(range, triggers)
  range.each do |num|
    result = ''
    triggers.each do |(text, divisor)|
      result << text if (num % divisor).zero?
    end
    puts result == '' ? num : result
  end
end

fizzbuzz(1..25, [['Fizz', 3], ['Buzz', 5]])

#
# We've introduced a new concept: triggers. A trigger is the pairing of a
# divisor and an output string. There is no official name for this pairing, due
# to FizzBuzz being a synthetic problem as opposed to a real-world problem, but
# it's not that uncommon. We create abstract models of data and processes, and
# these models contain things that need to be named. Often times there is a
# pre-existing name we can use, but sometimes not.
#


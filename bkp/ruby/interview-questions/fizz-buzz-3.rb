#
# https://www.tomdalling.com/blog/software-design/fizzbuzz-in-too-much-detail/
#

FIZZ = 'Fizz'
BUZZ = 'Buzz'

def divisible_by?(numerator, denominator)
  (numerator % denominator).zero?
end

1.upto(25) do |num|
  fizz = divisible_by?(num, 3)
  buzz = divisible_by?(num, 5)
  puts case
    when fizz && buzz then "#{FIZZ}#{BUZZ}"
    when fizz then FIZZ
    when buzz then BUZZ
    else num
  end
end



#
# https://www.tomdalling.com/blog/software-design/fizzbuzz-in-too-much-detail/
#
# DRY aka “Single Source Of Truth”.
#

print "Solution 1\n#{80.to_i * '-'}\n"
1.upto(25) do |num|
  fizz = (num % 3).zero?
  buzz = (num % 5).zero?

  puts case
    when fizz && buzz then 'FizzBuzz'
    when fizz then 'Fizz'
    when buzz then 'Buzz'
    else num
  end
end




def less_than_ten(num)
  if num >= 10
    raise ArgumentError, 'Argument must be less than 10.'
  end

  puts "Good number: #{num}"
end

less_than_ten(9)

begin
  less_than_ten(10)
rescue ArgumentError
  puts 'Exception caught.'
end


#
# The syntax is either:
#
#   raise <message>
#
# which uses `RuntimeError' by default, or
#
#   raise <ExceptionType> <message>
#
#

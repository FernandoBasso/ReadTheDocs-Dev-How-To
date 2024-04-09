#
# An _exception_ is a special kind of object. It is an instance of the class
# `Exception' or a descendant of that class.
#

print 'Type two integers to divide: '
dividend = gets.to_i
divisor = gets.to_i
res = nil
begin
  res = dividend / divisor
  puts "#{dividend} / #{divisor} = #{res}"
rescue ZeroDivisionError # <1>
  puts 'Trying to divide by zero?'
end

#
# Trap exactly “division by zero” errors.
#

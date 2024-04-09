#
# Creating our own exceptions and using them is a nice self-documenting
# technique one can benefit from.
#
class NoFooException < Exception
end

print 'Input string: '
input = gets.chomp

begin
  raise NoFooException if input.match(/foo/i)
rescue NoFooException => err # <1>
  puts 'Do not use ‘foo’ stuff as input.'
  puts err
end

#
# <1> Only `NoFooException` are trapped by the rescue clause.
#


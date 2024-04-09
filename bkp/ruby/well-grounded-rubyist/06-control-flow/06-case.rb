print 'Exit the program? (yes/no): '
answer = gets.chomp.downcase

case answer
when 'y', 'yes'
  puts 'Adios.'
  exit
when 'n', 'no'
  puts 'Continuing...'
  exit
else
  puts 'What‽'
  exit
end

#
# Produces `false` because those two 'foo's are two diferent object in memory.
#
#   'foo'.object_id == 'foo'.object_id
#
# Still,
#
#    'foo' == 'foo'
#
# produces `true' because Ruby assumes you want to compare the contents
# of the two strings.
#
# At least in Ruby 2.5.1, `show-doc String.==' and `show-doc String.==='
# produces the exact same help output, so, apparently `==' and `==='
# work exactly the same way.
#
# For strings and, indeed, for any object that doesn’t override it, `===' works
# the same as `==' (the basic string-equals-some-other-string test method). But
# other classes can define the threequal test any way they want.
#

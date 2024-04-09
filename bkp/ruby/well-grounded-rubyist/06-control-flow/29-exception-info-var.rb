#
# We capture the exception object into a variable and queries some methods
# on it to see possibly useful information.
#

def a_method(num)
  if num >= 10
    raise ArgumentError, "Please don't. Just don't."
    return
  end

  puts "Good number: #{num}"
end

begin
  a_method(10)
rescue ArgumentError => err
  puts err.message
  puts err.backtrace
  p err.class
end
# → Please don't. Just don't.
# → 29-exception-info-var.rb:8:in `a_method'
# → 29-exception-info-var.rb:16:in `<main>'
# → ArgumentError


#
# We say `rescue ArgumentError', not 'rescue ArgumentError.new'. Still, what
# gets "raised" is an intance of the class, not the class itself. In short,
# instances of exception classes get raised.
#

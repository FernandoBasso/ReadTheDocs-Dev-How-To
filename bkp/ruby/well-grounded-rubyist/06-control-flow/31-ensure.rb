def find_line(pattern)
  fh = File.open('file.txt')

  begin
    # Retrieve the first line.
    line = fh.gets # <1>
    raise ArgumentError unless line =~ /#{pattern}/i
  rescue ArgumentError
    puts "Invalid pattern: “#{pattern}”"
    raise
  ensure
    #
    # The `ensure' clause is _always_ executed, not giving a fuck as to whether
    # the exception was raised or not. We want to close the file handler no
    # matter what!
    #
    fh.close
  end

  return line # <2>
end

puts find_line('line 1')
# → line 1
#puts find_line('nope')
# Invalid pattern: “nope”
# Traceback (most recent call last):
#         1: from 31-ensure.rb:21:in `<main>'
# 31-ensure.rb:8:in `find_line': ArgumentError (ArgumentError)

#
# Pay attention scope. `line' is assigned inside the `begin' block, but
# it is acessible outside of it.
#
# It is important to note this, because for some other types of blocks, if you
# define a variable inside it, it cannot be accessed from outside. Yet, if you
# define it outside the block, change its value inside the block, then outside
# the block that change is visible.
#


1.times do
  # First defined here.
  foo = 'foo'
end
# Cannot access it here.
p foo # Error.

# Defined here.
bar = nil
1.times do
  # Changed here.
  bar = 'bar'
end
# CAN access it here.
p bar
# --> bar

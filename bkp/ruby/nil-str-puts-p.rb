Example = Class.new do
  # Note that @ivar is not defined, so, it produces nil when used.
  def show_ivar
    p "Again, ivar: #{@ivar}"   # <1>
    puts @ivar                  # <3>
    p @ivar                     # <3>
    puts @ivar.inspect          # <4>
  end
end

Example.new.show_ivar

#
# 1. nil inside a sting becomes an empty string when converted to string, so,
# we have one space after the result is "Again, ivar: ".
#
# 2. Prints nothing but an empty line.
#
# 3. p actually does puts @ivar.inspect, and it produces the string 'nil'.
#
# 4. Indeed, @ivar is nil.
#
# Running with ruby -w produces some warnings. That is expected because @ivar
# is nil, since it was not defined, purposefully, for this example.
#


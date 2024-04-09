#
# With methods, _each invocation_ creates a new scope.
#

class Example
  def a_method(a_jedi, recur = false)
    jedi_name = a_jedi
    p "self: #{self}"
    p "jedi_name: #{jedi_name}"

    if recur then
      puts 'recurring'
      a_method('Obiwan')
      puts 'after recursion'
      # `jedi_name' here still has the initial value `Yoda'
      p jedi_name
    end
  end
end

Example.new.a_method('Yoda', true)

# →  "self: #<Example:0x000056372e09b788>"
# →  "jedi_name: Yoda"
# →  recurring
# →  "self: #<Example:0x000056372e09b788>"
# →  "jedi_name: Obiwan"
# →  after recursion
# →  "Yoda"
#
# The object self is the same in the two iterations of the recursion.
# Still, `a' is scoped differently to each recursion iteration.
#
# The lines before `if recur then' are executed twice. Once before the
# recursion, then another time when recurring once, before `if recur then'
# line, which will be false the the recursion will stop.
#
# NOTE: self does not change, but the local variables have different values
# because each invocation of a_method creates a new scope, and a new value
# for a is passed on each iteration.
#

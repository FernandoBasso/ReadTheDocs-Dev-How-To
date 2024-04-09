
# LOCAL VARIABLES AND METHODS

# How does Ruby decide if `foo` is a variable or a method?
# If it sees `foo` has been assigned a value, it thinks it is a variable,
# otherwise it thinks it is a method.
#
# Many times when you see a `no method named ...` while trying to use
# a variable, it is very likely that it is a scope problem. You have that
# variable but in another scope. Example.

class Thing
  def foo
    bar
  end
end

obj = Thing.new
puts obj.foo
# → “undefined local variable or method bar”.

# Ruby didn't see an assignment to `bar`, therefore, it assumes `bar` is a
# method and attempts to call it.


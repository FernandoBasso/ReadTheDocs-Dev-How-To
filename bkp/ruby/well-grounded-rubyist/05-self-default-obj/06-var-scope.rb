class Example
  a_var = 'inside class body' # <1>
  def some_method
    a_var = 'inside method body'
    p a_var
  end
  p a_var # <2>
end

Example.new.some_method # <3>
# →  "inside class body"
# →  "inside method body"

#
# p a_var inside the body of the class (or a module, for that matter) is
# executed just by parsing the file.  Even if you never instantiate Example or
# run its instance method, <1> and <2> are executed nonetheless. Stuff inside
# methods are _not_ executed unless the method is actually invoked, though.
# Comment out <3> and see the results when you feed this file to ruby
# interpreter.
#

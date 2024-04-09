class AnExample
  p "inside the class body, self: `#{self}'"
  @ivar = "ivar inside the body of the class `#{self}'"
  p @ivar

  def show_ivar
    p "self inside the i_method: #{self}"
    p "ivar again:"
    p @ivar # will produce nil
  end
end

obj = AnExample.new
obj.show_ivar

#
# Note that just by doing SomeClass.new, any code inside it runs, even
# things hanging naked in the body of the class, like our puts statements.
#

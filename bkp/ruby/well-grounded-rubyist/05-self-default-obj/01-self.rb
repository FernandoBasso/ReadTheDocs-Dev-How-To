#
# Instance variables are self-bound.
#

class AnExample
  def get_ivar
    # Here,
    @my_ivar = 'IVAR inside method body'
  end

  @my_ivar = 'IVAR inside class body'

  # Using `self' to define the method on the class.
  def self.get_ivar # <1>
    @my_ivar
  end
end

p AnExample.new.get_ivar
# → "IVAR inside method body"

p AnExample.get_ivar
# → "IVAR inside class body"

p AnExample.new.class.get_ivar
# → "IVAR inside class body"

# 1. Use self.get_ivar in order to define the get_ivar method
# on the class itself.

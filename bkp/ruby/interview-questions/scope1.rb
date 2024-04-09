
#
# NOTE: Change `myvar` with `MYVAR` and see the difference.
#

myvar = 'global'

module Foo
  myvar = 'local'

  class Bar
    def thing1
      myvar
    end
  end
end

class Foo::Bar
  def thing2
    myvar
  end
end

p Foo::Bar.new.thing1
p Foo::Bar.new.thing2


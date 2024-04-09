#
# This example is a courtesy of Rodolfo Bandeira, @ule.
#

name = 'Master'

def a_method(name)
  name << ' Yoda'
end

a_method(name)

p name
# → "Master Yoda"

# Yeah, changing name inside a_method ended up making it changed
# in the toplevel as well.


foo = 'hey'

def let_us_see(thing)
  thing = ' you'
end

let_us_see(foo)

p foo

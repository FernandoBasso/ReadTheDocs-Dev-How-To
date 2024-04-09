#
# Let's see how class variables behave when inheritance comes into play.
#
# Class variables are HIERARCHY-SCOPED. All classes in a given hierarchy
# access the same class variable object in memory.
#

class Foo
  @@cvar = 'foo'

  def self.show_cvar
    @@cvar
  end
end

class Bar < Foo
  @@cvar = 'bar'
end

class Foo
  puts @@cvar # 'bar'
end

puts Foo.show_cvar
# →  'bar'

#
# When we assign to `@@cvar' inside `Foo', we assign to the one and only
# `@@cvar'. They are not just the same name in different clases. `@@cvar'
# in both the superclase `Foo' and in the child class `Bar' is the
# same thing (object) in memeory.
#
# Class variables are shared accross the hierarchy of classes.
#

#
# From now on, `@@cvar' has the value 'baz' in all three classes.
#
class Baz < Foo
  @@cvar = 'baz'
end

puts Foo.show_cvar
puts Bar.show_cvar
puts Baz.show_cvar
# → baz
# → baz
# → baz

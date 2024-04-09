Foo = Class.new do
  def initialize
    @stack = []
  end

  def <<(obj)
    @stack.push(obj)
  end

  def >>
    @stack.pop
  end
end

foo = Foo.new
foo << 'one' # Usage like an operator.
foo.<<('two') # Or as a method call with parentheses.
foo.<< 'three' # Or as a method call without parentheses.

p foo.>>
p foo.>>() # <1>
# → "three"
# → "two"



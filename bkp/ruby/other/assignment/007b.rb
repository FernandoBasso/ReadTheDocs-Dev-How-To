
public

  def foo
    'the force'
  end

  def bar
    'dark side'
  end

  # To call foo as a method, we either have to use parens.
  foo = foo()
  puts foo

  # Or using self. Just that the method must be set its visibility to public.
  bar = self.bar
  puts bar

  bar2 = self.bar
  puts bar2


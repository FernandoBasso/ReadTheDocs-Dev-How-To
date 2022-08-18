class Foo
  def to_proc
    [1, 2]
  end
end

foo = Foo.new

[].map(&foo)

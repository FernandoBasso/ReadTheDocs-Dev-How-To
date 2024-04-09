
def crappy()
  yield(1, 2, 3, 4, 5)
end

crappy do |foo, bar = 0, *rest, x, y|
  p foo, bar, rest, x, y
end


#
# We define the sponge params with `*rest', and use it as `rest' (without the
# *) inside the block. `rest' then is an _array_ containing zero or more
# sponged up arguments.
#
# BEWARE: If we use `*rest' (with the *) _inside_ the block, then it behaves
# like a normal argument name, and does not become an array. It just stands
# for one normal, and required argument.
#

def what(*args)
  # -w, `*' interpreted as argument prefix.
  p *args
end

what('foo')
# →  'foo', not ['foo']

def method1
  num = 13
  1.times do
    # `num' _is_ accessible here.
    p num
  end
end

method1
# → 13

def method2
  s = 'foo'
  1.times do
    s = 'bar'
  end
  # The change to `s' inside the block still holds. `s' is 'bar' here.
  p s
end

method2
# → 'bar'


def method3
  foo = 'foo'
  bar =  'bar'
  1.times do |bar|
    foo = 'foo changed'
    # Passed as argument to the block. Changes here remain here and are
    # not “visible” outside the block.
    bar = 'bar changed'
  end
  # Changed inside the block
  p foo
  # Still 'bar', not 'bar changed'. It was changed inside the block, but since
  # it was passed as a paremeter to the block, `bar' inside the block has nothing
  # to do with `bar' in the outer scope. The parameter overshadows the one from
  # the outer scope.
  p bar
end
# →  'foo'
# →  'bar'


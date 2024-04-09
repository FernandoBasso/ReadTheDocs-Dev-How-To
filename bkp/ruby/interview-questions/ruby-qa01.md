# Ruby Questions

- [&& vs and](#vs-and)
- [falsy values](#falsy-values)
- [hash keys to array sorted by length](#hash-keys-to-array-sorted-by-length)


## && vs and

```ruby
x = true and false # <1>
y = true && false  # <2>
```

1. `and` has lower precedence than `=`, therefore, `x = true and false` is the same as `(x = true) and false`, which means `true` is assigned to `x`.

2. `&&` has higher precedence than `=`, therefore, the value `false` (the result of `true && false`) is assigned to `y`.

In irb or pry, `x = true and false` print `false` because it is the result of the entire expression, but `x` gets assigned the value `true`.


## falsy values

Which expression will produce 'yes'‽

```ruby
true    ? 'yes' : 'nope' # 1
false   ? 'yes' : 'nope' # 2
nil     ? 'yes' : 'nope' # 3
1       ? 'yes' : 'nope' # 4
0       ? 'yes' : 'nope' # 5
'false' ? 'yes' : 'nope' # 6
''      ? 'yes' : 'nope' # 7
[]      ? 'yes' : 'nope' # 8
```

Only expressions 2 adn 3 will produce 'yes'. In Ruby, only `false` and `nil` are _falsy_ values. Everything else, including `[]`, `{}`, 0 and `''` are truthy.


## method call and expressions

```ruby
def identity(x)
  x
end

def sum(x, y)
  x + y
end

p identity 2    # <1>
# → 2

p identity(2)   # <2>
# → 2

p identity (2)  # <3>
# → 2


p sum 2, 2      # <4>
# → 4

p sum(2, 2)     # <5>
# → 4

p sum (2, 2)    # <6>
# → syntax error, unexpected ',', expecting ')'
# → p sum (2, 2)
```

No surprises in lines 1, 2, 4, and 5.

In line 3, because of the space between the name of the method and the opening parenthesis, Ruby treats `(2)` as an expression, and `(2)` is an expression which evaluates to 2. Therefore `identity (2)` is the same  as `identity 2`.

However, in line 6, again, because of the space between `sum` and `(`, Ruby thinks `(2, 2)` is an expression, but it is not a valid Ruby expression. Thus the error!


## namespaces classes and modules

```ruby
MYVAR = 'global'

module Foo
  MYVAR = 1

  class Bar
    def thing
      MYVAR
    end
  end
end

class Foo::Bar
  def thing2
    MYVAR
  end
end

p Foo::Bar.new.thing
# → "Foo Local"
p Foo::bar.new.thing
# → "Global"
```

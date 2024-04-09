# Ruby Scope

## instance variables vs class instance variables

```rb
class Foo
  @count = 0; # <E1>

  def initialize(num)
    @count  = num # <E2>
  end

  # <E3>
  def count
    @count
  end
end

foo = Foo.new(9);
p foo.count # <Q1>
# → 9, not 0.
```

Does Q1 print 9 or 0? Explain.
</summary>

<section>
It prints `0`.

In Ruby, `@some_var` (note the single `@`) can mean both an instance variable (_ivar_) or class instance variable (_civar_) depending on the scope where it is defined or used.

A `@some_var` syntax used inside an instance method (`count` in this case) will _always_ refer to an _instance variable_.

E1: Declares and initializes the `@count` _civar_ to 0. It is an instance variable of the `Foo` object (remember that classes are objects, instances of the Class class.)

E2: Then, `initialize` creates the `@count` _ivar_ and assigns it the value 9. Now we have two variables with the same name. One is a _civar_, the other is an _ivar_.

E3: Define a reader instance method `count` that simply returns `@count`. When that method is invoked, ruby has to decide whether to return the _ivar_ or the _civar_. Inside instance methods, a `@some_var` syntax will _always_ refer to an _ivar_.
</section>
</details>

## class instance variables accessors

How to write accessor methods for class instance variables

```ruby
p [10, 20, 30].each { |num| p num }
```

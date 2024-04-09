# Procs, Lambdas and Closures in Ruby

- [lambdas](#lambdas)
  - [an example](#an-example)


## Lambdas

This is how you create an anonymous function, or lambda:

```ruby
-> (num) { puts num + 10 }
```

But it does nothing. It is defined, and then, nothing is done with the just defined lambda. It is born and then simply vanishes.

We could define it and invoke it immediately.

```ruby
f = -> (num) { puts num + 10 }
f.call(5)
# → 15
```

PS: Remember you can also invoke a proc with two other (weird) syntaxes: `my_proc.(arg)` and `my_proc[arg]`.


### An Example
```ruby
def fizzbuzz(range, triggers)
  range.each do |num|
    result = ''
    triggers.each do |(text, predicate)|
      result << text if predicate.call(num)
    end
    puts result == '' ? num : result
  end
end

#
# Passing anonyous function predicates to test the trigger.
#
fizzbuzz(1..25, [
  ['Fizz', -> (n){ n % 3 == 0 }],
  ['Buzz', -> (n){ n % 5 == 0 }],
  ['Zazz', -> (n){ n < 10 }],
])
```


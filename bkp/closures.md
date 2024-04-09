# Closures, Ruby, JavaScript

- [intro](#intro)
- [references in scope](#references-in-scope)
- [a different way to explain closure references](#a-different-way-to-explain-closure-references)

## Intro

What is a _closure_‽

A closure a scope that is created when a function or piece of runnable code remembers the references (variables, constants, etc) that were in scope when it (the closure, the scope) was created.

We said _references_, meaning, the closure does not remember the value of a variable. That is, if a variable is in scope when the closure is created, and that variable later is assigned a new value, the closure will use that updated value, not the original value. Examples should make it clearer.

https://en.wikipedia.org/wiki/Closure_(computer_programming)


## references in scope

JavaScript:
```js
let str = 'Master';
let printStr = () => console.log(str);
str = 'Yoda';
printStr();
// → 'Yoda'
```

Note that the above JavaScript snippet prints 'Yoda', the value of `str` at the time `printStr` was called, not the value of `str` when `printStr` was defined. The closure remembers the references, not the actual values.

Ruby:
```rb
str = 'Master'
print_str = -> { puts str }
str = 'Yoda'
print_str.call
# → Yoda
```

It Ruby, it also prints 'Yoda', the value of `str` at the time `print_str` was invoked. The scope remembers the reference. If the value at that memory location changes, it is reflected when the proc or lambda is invoked.

NOTE: In Ruby, only procs and lambdas can access variables from the outer scopes. Methods cannot.

## a different way to explain closure references

First in JavaScript:
```js
// Declare a variable and give it a value.
let str = 'Master';

// Define the function `printStr` which makes use of the variable `str`. This
// effectively creates the closure scope.
let printStr = () => console.log(str);

// Now, before invoking `printStr`, we assign a different value to `str`.
str = 'Yoda';

// Now we invoke `printStr`. It prints `Yoda` because the closure cares about
// the reference `str`, not the value it was originally initialized with.
printStr();
// → 'Yoda'
```

And in Ruby:

```ruby
# Declare a variable and give it a value.
str = 'Master';

# Use a lambda so a closure comes into existence, and assign the lambda to
# `print_str`, so it can be invoked later.
print_str = -> { puts str };

# Now, before invoking `print_str`, we assign a different value
# to `str`.
str = 'Yoda';

# Now we invoke `print_str`. It prints `Yoda` because it cares about the
# reference `str`, not the value it was originally initialized with.
print_str();
# → 'Yoda'
```


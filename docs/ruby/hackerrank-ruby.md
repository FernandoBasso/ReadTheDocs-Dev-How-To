---
title: Ruby | HackerRank
description: Notes on Ruby Tutorial on HackerRank
---

# HackerRank Ruby Tutorial

Here are my notes on the HackerRank Ruby tutorial.
Many examples are my own (some based on their examples, some entirely mine).
Most notes and explanations are my own too and not just a verbatim copy from the website.

While solving the challenges, one can observe that to make things simple, many of them do not handle edge cases.
One such example is the `skip_animals()` method.
What if we skip more then the length of the input array?
Nothing seems to account for that in the description or the example.

In any case, most of the examples on this page try to be a bit careful with edge cases.
For example, in the `mask_article()`, we do not add `<strike>` tags around empty strings and have proper test cases for such scenarios.
We apply the same thoughtful care to in many other situations as well.

The solutions are implemented using the methods and approaches indicated in the description of the challenge, but keep in mind that many of them could d probably implemented in a better way if we used other ideas and concepts.

## Hello World
```rb
print "Hello, world!"
```

## self

`self` is the default receiver for messages.

## methods

Every object has methods.

```irb
> 1.even?
» false

> 1.odd?
» true

> (1..3).to_a.inject(&:+)
» 6
```

### Monkey Patch Integer and create `range?'

```rb
class Integer
  ##
  # Checks whether the receiver is between `x`
  # and `y`, inclusive.
  #
  def range?(x, y)
    self >= x && self <= y
  end
end

#
# Is 1 between 0 and 3‽
#
p 1.range?(0, 3)
```

## Accessing Array Elements

```irb
$ irb --simple-prompt

> xs = (-3..5).to_a
» [-3, -2, -1, 0, 1, 2, 3, 4, 5]

> xs.first
» -3

> xs[0]
» -3

> xs.last
» 5

> xs[-1]
» 5

> xs.take(3)
» [-3, -2, -1]

> xs.drop(3)
» [0, 1, 2, 3, 4, 5]
```

## Modifying Arrays

- `push` adds to the end.
- `insert` inserts at given index.
- `unshift` prepend elements to the beginning.

```irb
> xs = [1, 2]
» [1, 2]
```

At position 1, insert 10 and 20, moving 2 “to the right”.

```irb
> xs.insert(1, 10, 20)
» [1, 10, 20, 2]
```

 Add 3 and 4 to the end of the array.

```irb
> xs.push(3, 4)
» [1, 10, 20, 2, 3, 4]
```

Prepend -1 and 0 to the beginning of the array.

```irb
> xs.unshift(-1, 0)
» [-1, 0, 1, 10, 20, 2, 3, 4]
```

- `pop` deletes from the end.
- `shift` deletes from the beginning.
- `delete_at` deletes at given index.
- `delete` deletes all occurrences of given element.

```irb
> xs = (1..9).to_a
» [1, 2, 3, 4, 5, 6, 7, 8, 9]
```

Delete the last element. 9 is gone from `xs`.

```irb
> xs.pop
» 9
> xs
» [1, 2, 3, 4, 5, 6, 7, 8]
```

Delete the first element. 1 is gone from `xs`.

```irb
> xs.shift
» 1

> xs
» [2, 3, 4, 5, 6, 7, 8]
```

Delete at position 3. 5 is gone from `xs`.

```irb
> xs.delete_at(3)
» 5

> xs
» [2, 3, 4, 6, 7, 8]
```

Delete all occurrences of 6. We only have one 6, but it is gone from `xs`.

```irb
> xs.delete(6)
» 6
>
> xs
» [2, 3, 4, 7, 8]
```

## Filtering Arrays

Both `select` and `reject` return a new array without modifying the original array.

```irb
> xs = (1..9).to_a
» [1, 2, 3, 4, 5, 6, 7, 8, 9]

> xs.select(&:odd?)
» [1, 3, 5, 7, 9]

> xs.reject(&:odd?)
» [2, 4, 6, 8]
```

To modify the array in place, we use `keep_if` and `delete_if`.

```irb
> xs = (1..9).to_a
» [1, 2, 3, 4, 5, 6, 7, 8, 9]

> xs.keep_if { |x| x < 5 }
» [1, 2, 3, 4]

> xs
» [1, 2, 3, 4]
```

```irb
> xs = (1..9).to_a
» [1, 2, 3, 4, 5, 6, 7, 8, 9]
>
> xs.delete_if { |x| x < 5 }
» [5, 6, 7, 8, 9]
>
> xs
» [5, 6, 7, 8, 9]
```

Reject all elements divisible by 3:

```rb
> xs = (1..9).to_a
» [1, 2, 3, 4, 5, 6, 7, 8, 9]

> xs.reject { |n| n % 3 == 0 }
» [1, 2, 4, 5, 7, 8]
```

Or we can create a proc and convert  it to a block with the `&` trick:

```rb
div_by_3 = Proc.new do |n|
  n % 3 == 0
end

p (1..9).to_a.reject(&div_by_3)
#
# → [1, 2, 4, 5, 7, 8]
##
```

Select only numbers divisible by 4:

```rb
div_by_4 = Proc.new { |n| n % 4 == 0 }

p (1..9).to_a.select(&div_by_4)
#
# → [4, 8]
##
```

Keep only negative numbers:

```rb
is_negative = lambda { |n| n < 0 }

p (-3..3).to_a.reject(&is_negative)
#
# → [0, 1, 2, 3]
##
```

Keep only positive numbers:

```rb
is_positive = -> (n) { n > 0 }
p (-3..3).to_a.select(&is_positive)
#
# → [1, 2, 3]
##
```

Careful with precedence:

```rb
(-3..3).reject({ |n| n < 0 })
#
# syntax error, unexpected '|', expecting '}'
# possibly useless use of < in void context
# syntax error, unexpected '}', expecting end-of-input
##
```

The parentheses on `reject()` causes problems. Drop them:

## Hashes

Create an empty hash:

```rb
h = {}
g = Hash.new
```

Create hash with all keys having 1 as default value:

```rb
> h = Hash.new(1)
» {}

> h[:k]
» 0

> g = {}
» {}

> g.default = 1
» 1

> g[:foo]
» 1
```

Two syntaxes:

```rb
> yoda = { 'name' => 'Yoda', 'skill' => 'The Force' }
» {"name"=>"Yoda", "skill"=>"The Force"}

> luke = { :name => 'Luke', :skill => 'Fast Learner' }
» {:name=>"Luke", :skill=>"Fast Learner"}

> ahsoka = { name: 'Ahsoka Tano', skill: 'Lightsaber' }
» {:name=>"Ahsoka Tano", :skill=>"Lightsaber"}
```

We can iterate over hash keys with `each`:

```rb
jedi {
  id: 1,
  name: 'Yoda',
  skill: 'The Force'
}

jedi.each do |k, v|
  p k v
  p v
end
#
# → :id
# → 1
# → :name
# → "Yoda"
# → :skill
# → "The Force"
##

jedi.each do |arr|
  p arr
end
#
# → [:id, 1]
# → [:name, "Yoda"]
# → [:skill, "The Force"]
##
```

Other cool stuff:

```irb
> h = { 1 => 1, 2 => 4, 3 => 9, 4 => 16, 5 => 25 }

> h.keep_if { |k, v| k > 3 }
» {4=>16, 5=>25}
```

```irb
h = { 4 => 16, 5 => 25 }
> h[:foo] = :bar
» :bar
> h
» {4=>16, 5=>25, :foo=>:bar}

> h.delete_if { |k, v| k.is_a? Integer }
» {:foo=>:bar}
```

## unless

```rb
class User
  def initialize(name, is_admin)
    @name = name
    @admin = is_admin
  end

  def say_hello
    p "Hello, #{@name}!"
  end

  def admin?
    @admin
  end
end

users = [
  User.new('Yoda', true),
  User.new('Ahsoka', false),
  User.new('Aayla', false),
]

users.each do |user|
  unless user.admin?
    user.say_hello
  end
end
#
# → "Hello, Ahsoka!"
# → "Hello, Aayla!"
##
```

## loop, break if

```rb
class Coder
  def initialize(name)
    @name = name
    @level = 0;
  end

  def level
    @level
  end

  def master?
    @level >= 100
  end

  def practice
    @level = @level + 10
    p "Got to level #{@level}"
  end
end

##
# Practice until you become a master.
#
coder = Coder.new('Aayla Secura')

loop do
  break if coder.master?
  coder.practice
end
#
# → "Got to level 10"
# → "Got to level 20"
# → "Got to level 30"
# → "Got to level 40"
# → "Got to level 50"
# → "Got to level 60"
# → "Got to level 70"
# → "Got to level 80"
# → "Got to level 90"
# → "Got to level 100"
##
```

What about this oneliner?

```rb
developer = Coder.new('Ahsoka Tano')
developer.practice until developer.master?
#
# → "Got to level 10"
# → "Got to level 20"
# → "Got to level 30"
# → "Got to level 40"
# → "Got to level 50"
# → "Got to level 60"
# → "Got to level 70"
# → "Got to level 80"
# → "Got to level 90"
# → "Got to level 100
##
```

## group_by

Note how in the first two cases numbers are grouped into `true` and `false`, while in the third example, they are grouped into `0` and `1`.
The comparison inside the block causes the grouping to be in a certain way.
With the first two cases, the block returns a boolean, while in the third case, it returns ether 0 or 1.

```irb
> (1..5).group_by(&:odd?)
» {true=>[1, 3, 5], false=>[2, 4]}

> (1..5).group_by { |n| n % 2 == 0 }
» {false=>[1, 3, 5], true=>[2, 4]}

> (1..5).group_by { |n| n % 2 }
» {1=>[1, 3, 5], 0=>[2, 4]}
```

### Hash Gotcha!

:::::{container} qanda
::::{container} question
```rb
yoda = { name: 'Yoda', level: 100 }
p yoda.level
#
# NoMethodError (undefined method `level' for
# {:name=>"Yoda", :level=>100}
##
```
::::

::::{container} answer
`yoda.level` syntax is trying to send the message `level` (call the method `level`) to the receiver `yoda`.

What we need is to access the symbol:

```rb
p yoda[:level]
```
::::
:::::

To be a jedi master, your skill level must be >= 80.
A padawan has skill level < 80.

```rb
jedis = {
  'Yoda': 100,
  'Ahsoka Tano': 93,
  'Aayla Secura': 91,
  'Luke Skywalker': 93,
  'Anakin Skywalker': 60
}

groups = jedis.group_by do |_k, v|
  v < 80 ? :padawan : :master
end

ap groups[:padawan]
#
# → [
# →     [0] [
# →         [0] :"Anakin Skywalker",
# →         [1] 60
# →     ]
# → ]
##

ap groups[:master]
#
# → [
# →     [0] [
# →         [0] :Yoda,
# →         [1] 100
# →     ],
# →     [1] [
# →         [0] :"Ahsoka Tano",
# →         [1] 93
# →     ],
# →     [2] [
# →         [0] :"Aayla Secura",
# →         [1] 91
# →     ],
# →     [3] [
# →         [0] :"Luke Skywalker",
# →         [1] 93
# →     ]
# → ]
##

```

## Arrays

Create an empty array:

```rb
xs = []
ys = Array.new
```

An array with 3 single `nil` elements:

```rb
xs = [nil, nil, nil]
ys = Array.new(3)
zs = Array.new(3, nil)
```

An array with 5 elements whose values are false.

```rb
xs = [false, false, false, false, false]
ys = Array.new(5, false)
```

We can use subscript notation with range syntax to return a slice of the array:

```rb
> xs = (1..9).to_a

> xs[0..3]
» [1, 2, 3, 4]

> xs[0...3]
» [1, 2, 3]

>xs[2, 5]
» [3, 4, 5, 6, 7]
```

## Currying

> Currying is a technique in which a function accepts `n` parameters and turns it into a sequence of `n` functions, each of them take 1 parameter.

```rb
add = ->(x, y) { x + y }

ap add.call(-1, 1)
#
# → 0
##

##
# Partially apply `call` to 1.
#
add1 = add.curry.call(2)

ap add1.call(10)
#
# → 11
##
```

Remember we can use the `.()` short syntax (among others, more obscure 😱).

```rb
add = ->(x, y) { x + y }

ap add.(-1, 1)
#
# → 0
##

##
# Partially apply `add` to 1.
#
add1 = add.curry.(2)

ap add1.(10)
#
# → 11
##
```

## Lazy

Ruby 2.0 introduced lazy evaluation, which can work with potentially infinite data structures (more or less like in Haskell 💖 λ).

Some initial, simple examples:

The first five positive integers:

```irb
> 1.upto(Float::INFINITY).lazy.first(5)
[1, 2, 3, 4, 5]
```

The first five negative integers:

```irb
>> -1.downto(-Float::INFINITY).lazy.first(5)
=> [-1, -2, -3, -4, -5]
```

The first eight odd positive numbers:

```irb
1.upto(Float::INFINITY).lazy.select {|n| n.odd?}.first(8)
[1, 3, 5, 7, 9, 11, 13, 15]
```

10 negative even integers starting at -1e5:

```irb
(-1e5.to_i).downto(-Float::INFINITY).lazy.select {|n| n.even?}.first(10)
=> [-100000,
 -100002,
 -100004,
 -100006,
 -100008,
 -100010,
 -100012,
 -100014,
 -100016,
 -100018]
```

Note that we do `.to_i` because exponential notation makes the value a `Float`, not a `Integer`, and `upto` and `downto` work on `Integer` (not `Float`). See:

```irb
>> -1e1.class
=> Float
>> (-1e1.to_i).class
=> Integer
```

### Lazy Array of Powers

An example with lazy to generate an array of powers:

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/e11_lazy_pow.rb
:language: ruby
```

### Lazy Array of Palindromic Primes

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/is_palindrome.spec.rb
:language: ruby
```

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/is_palindrome_v1.rb
:language: ruby
```

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/is_prime.spec.rb
:language: ruby
```

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/is_prime_v1.rb
:language: ruby
```

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/palindromic_primes.spec.rb
:language: ruby
```

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/palindromic_primes_v1.rb
:language: ruby
```

## Blocks

This is what HackerRank expect in that “fill the blanks” ill-explained exercise:

```rb
def factorial
  yield
end

n = gets.to_i
factorial do
  puts (1..n).inject(:*)
end
```

Just for kicks, here's a recursive definition of factorial:

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/factorial_v1.rb
:language: ruby
```

And this using `inject`:

```rb
##
# Computes the factorial of `n`.
#
# This approach uses `inject` cleverly :)
#
# ASSUME: `n` is an integer greater than or equal to 1.
#
def factorial(n)
  (1..n).inject(&:*)
end
```

Blocks are not objects (a rare exception when something is not an object in Ruby) therefore they can't be referenced by variables.

## Procs

Procs are like “saved blocks”, except that procs *are* objects (unlike blocks).
They can be bound to a set of local variables.

```rb
##
# A function-like object that takes one numeric parameter and
# increments it by 1.
#
add1 = proc { |n| n + 1 }

##
# Takes an `x` value and a proc and call the proc with the value.
#
def f(x, a_proc)
  a_proc.call(x)
end

p f(0, add1)
```

We run the passed proc with `.call()`. Other way would be `a_proc.(x)` and `a_proc[x]` and `a_proc === 1`.
You don't believe ~~in the force~~ me, do you‽
See it for yourself:

```irb
>> f = proc {|n| n + 1}
=> #<Proc:0x000055ca6bc6b200 (pry):1>
>> f.call(1)
=> 2
>> f.(1)
=> 2
>> f[1]
=> 2
>> f === 1
=> 2
```

This is the challenge in HackerRank:

```rb
def square_of_sum (my_array, proc_square, proc_sum)
  sum = proc_sum.call(my_array)
  proc_square.call(sum)
end

proc_square_number = proc { |x| x * x }
proc_sum_array     = proc { |xs| xs.inject(&:+) }

my_array = gets.split().map(&:to_i)

puts square_of_sum(my_array, proc_square_number, proc_sum_array)
```

## Lambdas

A method that returns a lambda using the affectionately called *stabby* lambda syntax.

```rb
def add1(x)
  -> { x + 1 }
end

p add1.call(0)
```

Note that `x` is in scope inside the lambda braces.

Define `add1` without the method surrounding the returned lambda.
Again, using the stabby syntax:

```rb
add1 ->(x) { x + 1 }
p add1.call(x)
```

No spaces between `->` and the opening parenthesis.
It has to do with Rubocop default rule checking for that space.
It seems Ruby 1.8 would produce an error if there was a space there.
Later versions started allowing it, but now some people think it should always be without the space as a matter of style.
See:

- [No rule for space after stab (Rubocop issue)](https://github.com/rubocop/ruby-style-guide/issues/603).
- [Rubocop Layout/SpaceInLambdaLiteral](https://docs.rubocop.org/rubocop/cops_layout.html#layoutspaceinlambdaliteral)

Using `lambda` keyword:

```rb
add1 = lambda { |x| x + 1 }
```

- [What's the difference between a proc and a lambda in Ruby? (StackOverflow)](https://stackoverflow.com/questions/1740046/whats-the-difference-between-a-proc-and-a-lambda-in-ruby).

Example for calculating area of rectangle and triangle:

```rb
##
# A lambda that computes the area of a rectangle.
#
# The math formula is:
#
#   A = base * length
#
area ->(b, l) { b * l }

#
# The area of a triangle is computed by the following formula:
#
#   A = 1/2 * base * length
#

area_rectangle = area(2, 3).call
area_triangle = (1 / 2) * area(2.0, 3).call

p area_rectangle
p area_triangle
```

1/2 is 0.5, but in Ruby operations with integers results in integers.
`1 / 2` is `0` 😲, unless we make at least one of the numbers a decimal, like `1.0 / 2`, which then prints `0.5`.
That is why we pass 2.0 above, so that we have at least one decimal/floating point number, which in turn causes all the others to be treated as floats as well.

Finally, this is the challenge on HackerRank:

```rb
# Write a lambda which takes an integer and square it.
square      = ->(x) { x * x }

# Write a lambda which takes an integer and increment it by 1.
plus_one    = lambda { |x| x + 1 }

# Write a lambda which takes an integer and multiply it by 2.
into_2      = lambda { |x| x * 2 }

# Write a lambda which takes two integers and adds them.
adder       = ->(x, y) { x + y }

# Write a lambda which takes a hash and returns an array of hash values.
values_only = lambda { |h| h.values }

input_number_1 = gets.to_i
input_number_2 = gets.to_i
input_hash = eval(gets)

a = square.(input_number_1)
b = plus_one.(input_number_2)
c = into_2.(input_number_1)

d = adder.(input_number_1, input_number_2);
e = values_only.(input_hash)

p a
p b
p c
p d
p e
```

## Closures

A closure is a function or method that can be passed around like objects and remembers scope after parent scope function has returned.
Blocks, procs and lambdas are closures in Ruby.

Example challenge from HackerRank:

```rb
def block_message_printer
  message = "Welcome to Block Message Printer"
  if block_given?
    yield
  end
  puts "But in this function/method message is :: #{message}"
end

message = gets
block_message_printer { puts "This message remembers message :: #{message}" }

#####################################################################################

def proc_message_printer(my_proc)
  message = "Welcome to Proc Message Printer"
  my_proc.call(message) # Call my_proc
  puts "But in this function/method message is :: #{message}"
end


my_proc = proc { puts "This message remembers message :: #{message}" }
proc_message_printer(my_proc)

######################################################################################

def lambda_message_printer(my_lambda)
  message = "Welcome to Lambda Message Printer"
  my_lambda.call # Call my_lambda
  puts "But in this function/method message is :: #{message}"
end

my_lambda = -> { puts "This message remembers message :: #{message}" }
lambda_message_printer(my_lambda)
```

### Example 1 (remembers x)

```rb
def add1(f)
  f.call
end

x = 1

fn = -> { x + 1 }

#
# `x` is in the top level scope. Yet, `fn` can remember its value
# when called inside `add1`.
#
# When we define `fn`, it references `x`. `fn` will remember the
# value of `x`

p add1(fn)
# → 2
```

### Example 2 (error)

```rb
def add1(f)
  f.call
end

#
# We intentionally do NOT define `x` before creating the lambda/closure.
#
# x = 1 (we don't do this on purpose).
#

#
# `x` is not yet defined. It is defined only later.
#
fn = -> { x + 1 }

x = 10
#
# Useless assignment to variable - `x`
##

p add1(fn)
#
# undefined local variable or method `x'
#
# Because no `x` was defined before we defined the lambda/closure,
# we get an error saying `x` does not exist.
##
```

### Example 3 (remembers 2nd x)

```rb
def add1(f)
  f.call
end

x = 1

fn = -> { x + 1 }

x = 10

p add1(fn)

#
# Which `x` will the closure remember?
#
# x = 10 will be remembered. The output is 12.
#
# So, if `x` is defined earlier, and then reassigned, it remembers
# its last value. But as we saw in the previous example, if it is
# only defined after we declare the closure, then it throws an error.
#
```

:::{admonition} tip
Take a look at the source code in [Gitlab](https://gitlab.com/devhowto/Dev-How-To/-/tree/main/src/ruby/hackerrank-ruby-tutorial) and compare the closure examples with Ruby and JavaScript.
:::

## Currying and Partial Application

This is manual currying. We return a lambda that takes one parameter, which returns another lambda that takes another parameter, which then returns the final result.

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/part_application_v1.rb
:language: ruby
```

Another example, this one from the HackerRank challenge:

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/part_application_v2.rb
:language: ruby
```

## Arguments and Splat Operator

We can use `*` splat operator in many situations.
Here's one case where it works as the rest `...params` in ECMAScript, that is, it collects all parameters into an array.
Zero params would mean an empty array.

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/args_splat_1.rb
:language: ruby
```

Example from HackerRank challenge:

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/args_splat_2.rb
:language: ruby
```

## Keyword Arguments

Before Ruby 2, people used the “options (or config) object” pattern (like we do in ECMAScript) to provide multiple parameters to a function/method in a saner way than having too many positional parameters.
Ruby 2 introduced *keyword arguments*.

- [Keyword Arguments in Ruby 2.0 (brainspec.com)](http://brainspec.com/blog/2012/10/08/keyword-arguments-ruby-2-0/).

Here's one example where we use `tries` keyword argument defaulting to 2.
It is a made-up example show to showcase keyword arguments (and as a by-product, also show how to mock `Kernel#random`).

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/keyword_args_1.rb
:language: ruby
```

### Temperature Converter

This is my solution for the temperature converter challenge using keyword arguments.

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/temperature_converter_v2.rb
:language: ruby
:lines: 9-
```

## String Indexing

### Accessing

Consider this string:

```irb
>> s = 'Hello!'
=> "Hello!"
```

Get the char at the last position:

```irb
>> s[s.size - 1]
=> "!"

>> s[-1]
=> "!"
```

Last but one (penultimate, second last):

```irb
>> s[-2]
=> "o"
```

Last but 2 (antepenultimate, third last):

```irb
>> s[-3]
=> "l"
```

First:

```irb
>> s[0]
=> "H"
```

From first to fourth, inclusive:

```irb
>> s[0,4]
=> "Hell"
```

A range from 0 to fourth means five characters:

```irb
>> s[0..4]
=> "Hello"

A range from the first to the last one (the entire string):

```irb
>> s[0..-1]
=> "Hello!"
```

Looks like it would mean “from first to the last one”, but nope...

```irb
>> s[0,-1]
=> nil
```

From the last position, get the next zero chars:

```irb
>> s[-1,0]
=> ""
```

From the last position, get the next one char:

```irb
>> s[-1,1]
=> "!"
```

From the last position, get the next two chars (except from the last position, we can only get that last one, there no next two, only the single one):

```irb
>> s[-1, 2]
=> "!"
```

BEWARE! Our string has length 6, and indexed from 0 to 5 (not 0 to 6 or 1 to 6).

```irb
>> s.size
=> 6
```

But compare these results. Why don't `[6,1]` and `[6,3]` return `nil`?:

```irb
>> s[6]
=> nil

>> s[6,1]
=> ""

>> s[6,3]
=> ""
```

Only after index 6 we get `nil` with the interval:

```irb
>> s[7]
=> nil

>> s[7,3]
=> nil
```

### Mutating, Changing

Let's start with this string:

```irb>> s = 'Hello!'
=> "Hello!"
```

Replace the last char:

```irb
>> s[-1] = '.'
=> "."
>> s
=> "Hello."
```

Delete the last char (the “.”):

```irb

```

Append ' World!':

```irb
>> s[5,0] = ' World!'
=> " World!"
>> s
=> "Hello World!"
```

Add a comma after the 5th position without replacing/overriding any other char, effectively shifting the space after “Hello” and the rest of the string to the right:

```irb
>> s[5,0] = ','
=> ","
>> s
=> "Hello, World!"
```

Position 7 is “W”.
Let's replace from that position, all characters in “World”, with “Ruby”.
“World” has 5 chars.
So, from position 7, replace the next 5 chars:

```irb
>> s = 'Hello, World!'
=> "Hello, World!"

>> s[7]
=> "W"

>> s[7,5] = "Ruby"
=> "Ruby"

>> s
=> "Hello, Ruby!"
```

### Serial Average

Note that `-10.00` and `-20.00` have 6 chars.
Also note we index from 0 to 3, then from 3 to 6, which combined makes 9. That is why we start at 0, then at 3, then at 9.

```irb
>> s = '002-10.00-20.00'
=> "002-10.00-20.00"

>> s[0,3]
=> "002"

>> s[3,6]
=> "-10.00"

>> s[9,6]
=> "-20.00"
```

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/serial_avg_v1.rb
:language: ruby
```

## String Iteration

Before ruby 1.9, strings where enumerable, and we could do `my_str.each` (from `Enumerable`).
There were some problems with it because of encoding and people could not iterate over bytes without resorting to tricks.

Since ruby 1.9, the `String` class does not bear a `each` method anymore.
Instead, we have `each_char`, `each_byte`, `each_codepoint`, and `each_line` (among other string methods, of course).
It is said `each_char` is more performant than `[]` and character indexing.

### Count Multibyte Chars

Here's the HackerRank challenge about counting multibyte chars in a string.

Unit tests:

```rb
describe 'count_mbc()' do
  it 'should work with empty string' do
    expect(count_mbc('')).to eq 0
  end

  it 'should work with a single multibyte char' do
    # 0x2714
    expect(count_mbc('✔')).to eq 1
    # 0x0001f4a9
    expect(count_mbc('💩')).to eq 1
  end

  it 'should work with multiple multibyte chars' do
    expect(count_mbc('✔💩')).to eq 2
  end

  it 'should work with mixed ASCII-like and multibyte chars' do
    expect(count_mbc('lambda λ')).to eq 1
    expect(count_mbc('¥1000')).to eq 1
    expect(count_mbc('May the ✔ source be 💩 with λ you!')).to eq 3
  end
end
```

Version 1 using single monolithic method:

```rb
##
# Counts the number of multibyte chars in the string `s`.
#
# Example: 'ab λ' has four chars, but only 'λ' is a multibye char.
# The others are ASCII-compabitle, single byte chars (including
# the space). Therefore, 'ab λ' has 1 multibyte char.
#
def count_mbc(s)
  num_multibyte_chars = 0

  s.each_char do |c|
    num_bytes = 0

    c.each_byte do |b|
      num_bytes += 1
    end

    if num_bytes > 1
      num_multibyte_chars += 1
    end
  end

  num_multibyte_chars
end
```

Version 2 using helper method:

```rb
##
# Counts the number of bytes in the char `c`.
#
def count_bytes(c)
  count = 0

  c.each_byte do |b|
    count += 1
  end

  count
end

##
# Counts the number of multibyte chars in the string `s`.
#
# Example: 'ab λ' has four chars, but only 'λ' is a multibyte char.
# The others are ASCII-compatible, single byte chars (including
# the space). Therefore, 'ab λ' has 1 multibyte char.
#
def count_mbc(s)
  num_multibyte_chars = 0

  s.each_char do |c|
    if count_bytes(c) > 1
      num_multibyte_chars += 1
    end
  end

  num_multibyte_chars
end

```

## String Methods I

- [ HackerRank String Methods ‘process_text’ challenge](https://www.hackerrank.com/challenges/ruby-strings-methods-i/problem).
- [String Ruby 3.1.2 docs](https://ruby-doc.org/core-3.1.2/String.html).

`String#chomp` removes `\n`, `\r` and `\r\n` from the end of a string (unless the default separator has been changed to something else).

`String#chop` removes the last char, and note that `\n`, `\r` and `\r\n` are all threated as one single char.

`String#strip` is like `trim()` in some other languages, which removes leading and trailing whitespace.

Here's my solution to the process text challenge:

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/process_text.spec.rb
:language: ruby
```

```{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/process_text_v1.rb
:language: ruby
:lines: 5-
```

## String Methods II

- [HackerRAnk String Methods II ‘mask_article()’ challenge](https://www.hackerrank.com/challenges/ruby-strings-methods-ii)
- [String literals docs](https://ruby-doc.org/core-3.1.2/doc/syntax/literals_rdoc.html#label-String+Literals)

We'll use `includes?` and `gsub`.

:::{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/mask_article_v1.rb
:language: ruby
:lines: 5-
:::

## Enumerables ‘each_with_index’

- [HackerRank Enumerables ‘each_with_index’ ‘skip_animals’ challenge](https://www.hackerrank.com/challenges/ruby-enumerable-each-with-index)

:::{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/skip_animals_v1.rb
:language: ruby
:lines: 9-
:::

## rot13, map, collect

- [HackerRank ‘collect’ ‘rot13’ challenge](https://www.hackerrank.com/challenges/ruby-enumerable-collect)

:::{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/decrypt_msgs_v1.rb
:language: ruby
:lines: 5-
:::

The `String#tr` version above is the same one I once learned with the `tr` command line:

```shell-session
$ printf '%s' aBc | tr 'A-Za-z' 'N-ZA-Mn-za-m'
nOp
```

Or, using bash Here Strings:

```shell-session
$ tr 'A-Za-z' 'N-ZA-Mn-za-m' | <<< 'aBc'
nOp
```

## Enumerable reduce, inject

- [HackerRank ‘reduce/inject’ ‘sum_terms’ arithmetico-geometric sequence challenge](https://www.hackerrank.com/challenges/ruby-enumerable-reduce)
- [Arithmetico-geometric sequence (Wikipedia)](https://en.wikipedia.org/wiki/Arithmetico%E2%80%93geometric_sequence)

:::{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/arith_geometric_seq_sum.spec.rb
:language: ruby
:::

:::{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/arith_geometric_seq_sum_v1.rb
:language: ruby
:lines: 5-
:::

## Enumerables any, all, none, find

- [HackerRank Enumerable any, all, none and find HackerRank challenge](https://www.hackerrank.com/challenges/ruby-enumerable-any-all-none-find/).
- [Enumerable Docs](https://ruby-doc.org/core-3.1.2/Enumerable.html).

### any

Is there *any* even number in the array?

```irb
>> [1, 3, 5].any? { |x| x.even? }
=> false

>> [1, 3, 8, 5].any? { |x| x.even? }
=> true
```

Are any values in the hash of the type `Integer`?

```irb
>> h = { one: 1, two_1: 2.1 }

>> h.any? { |k, v| v.is_a? Integer }
=> true
```

Are any of the keys a `Symbol`?

```irb
>> h = { 'one' => 1, :two => 2 }

>> :foo.is_a? Symbol
=> true

>> h.any? { |k, v| k.is_a? Symbol }
=> true
```

### all

Do *all* elements satisfy the predicate? E.g, are all elements integers?

```irb
>> [1, 1.1, 2].all? { |x| x.is_a? Integer }
=> false

>> [1, 2].all? { |x| x.is_a? Integer }
=> true
```

### none

Are *none* of the elements are `nil`:

```irb
>> [:foo, nil, :bar].none? { |e| e.nil? }
=> false

>> [:foo, :bar].none? { |e| e.nil? }
=> true
```

**TIP**: Remember we could simply do `arr.none?(&:nil?)`.

### find
Can we *find* an element that is greater than 5?

```irb
>> (1..5).find { |x| x > 5 }
=> nil

>> (1..6).find { |x| x > 5 }
=> 6
```

`find` returns `nil` if it can't find what we are looking for, or it returns the thing we are looking for if we actually find it.

### Solution for the challenge

:::{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/enumerables1.rb
:language: ruby
:lines: 3-
:::

## String Encoding

- [HackerRank string encoding challenge](https://www.hackerrank.com/challenges/ruby-strings-encoding/).
- [Ruby docs on String#encode](https://ruby-doc.org/core-3.1.2/String.html#method-i-encode).

Useful snippets regarding encoding:

- `str.encoding`
- `str.encoding.name`
- `str.encode(dst_encoding, **options)`
- `str.encode(dst_encoding, src_encoding, **options)`

Solution:

:::{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/transcode_v1.rb
:language: ruby
:lines: 3-
:::

NOTE: The solution presented here DOES NOT pass HackerRank tests.
The comments in the challenge explain the tests are probably buggy.
To make tests pass, use `s.force_encoding('UTF-8')`.

See these quotes from the [challenge comments](https://www.hackerrank.com/challenges/ruby-strings-encoding/forum):

> First of all, the exercise is wrong.
> `force_encoding` forces the encoding of a string without changing the internal byte representation of the string.
> `encode` changes the encoding and the internal byte representation.
> The exercise asks for transcode a string, but `force_encoding` isn't a transcode method but encode actually is a transcode method.
>
> — [Juan Manuel Furattini](https://www.hackerrank.com/juan_furattini)

> this exercise passes only with a wrong answer (as the official doc states, `force_encoding` doesn't change the internal byte representation).
> `force_encoding` should be used only when a string's internal representation doesn't match the Encoding information associated to the string.
> K-
>
> — [Torumori](https://www.hackerrank.com/torumori)

## Methods Intro

- [HackerRank methods intro challenge](https://www.hackerrank.com/challenges/ruby-methods-introduction/).

Global methods like:

```rb
def hello
  'Hello!'
end

p hello
#
# → "Hello!"
##
```

Are the same as:

```rb
class Object
  private

  def hello
    'Hello!'
  end
end

p Object.send(:hello)
#
# → "Hello!"
##
```

:::{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/methods_v1.rb
:language: ruby
:::

We already did a `prime?` method before. Here's another version, though:

:::{literalinclude} /../src/ruby/hackerrank-ruby-tutorial/is_prime_v2.rb
:language: ruby
:lines: 3-
:::

## Method Arguments

- [HackerRank method arguments challenge](https://www.hackerrank.com/challenges/ruby-methods-arguments).

The challenge calls this method `take`, but it sounds more like `drop`, because it *drops*, or *skips* the first `skip` elements:

```rb
def take(xs, skip = 1)
  xs[skip, xs.size]
end

>> take([1, 2, 3, 4], 2)
=> [3, 4]
```

Anyway, the above method `take` *skips* `skip` elements and return the rest of the array.

See what `drop` and `take` do in Haskell:

```text
ghci> drop 2 [1..5]
[3,4,5]

ghci> take 2 [1..5]
[1,2]
```

Made a [comment about this naming thin in the discussions for this challenge](https://www.hackerrank.com/challenges/ruby-methods-arguments/forum).


## Ruby Enumerable Intro

- [HackerRank colors each challenge](https://www.hackerrank.com/challenges/ruby-enumerable-introduction/).

```rb
def iterate_colors(colors)
  color_values = []
    
  colors.each do |color|
    color_values << color
  end
    
  color_values
end


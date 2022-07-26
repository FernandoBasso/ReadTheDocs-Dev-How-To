---
title: Ruby | HackerRank
description: Notes on Ruby Tutorial on HackerRank
---

# HackerRank Ruby Tutorial

Here are my notes on the HackerRank Ruby tutorial.
Many examples are my own (some based on their examples, some entirely mine).
Most notes and explanations are my own too and not just a verbatim copy from the website.

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

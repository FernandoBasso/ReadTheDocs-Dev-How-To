---
title: Ruby | HackerRank
description: Notes on Ruby Tutorial on HackerRank
---

# Ruby HackerRank Tutorial

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

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

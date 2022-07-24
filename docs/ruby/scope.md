---
title: Scope | Ruby
description: Some notes and tips about scope in Ruby regarding files, methods, procs, lambdas, classes and modules.
---

# Scope | Ruby

## File Local Variables

A **lowercase or snake_case local variable is local to the file it is defined** (contrary to Capitalized Modules and Classes, which are shared with other files when required).

Here, `add` is a local variable to `add.rb` file.

```rb
#
# add.rb
#
add = lambda do |acc, n|
  acc + n
end
```

```rb
#
# sum_arr.rb
#
require_relative 'add'

(1..5).inject(0, &add)
```

```shell-session
$ ruby -w sum_arr.rb
undefined local variable or method `add'
```

In this case, the `add()` lambda defined in `add.rb` **will not be available** in `sum_arr.rb`, even though the file is properly *required*.

We could make the lambda uppercase, then it would work:

```
Add = lambda ...

(1..5).inject(0, &Add)
```

But defining a lambda in the top level scope of a module with a Capitalized identifier does not read like idiomatic ruby code.

Perhaps making a proper module makes it a little more idiomatic:

```rb
#
# arith.md
#
module Arith
  Add = lambda do |acc, e|
    acc + e
  end
end
```

```rb
#
# sum_arr.rb
#
require_relative 'arith'

p (1..5).inject(0, &Arith::Add)
```

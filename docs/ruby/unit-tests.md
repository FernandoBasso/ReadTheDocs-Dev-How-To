---
title: Unit Tests | Ruby
description: Some notes, tips and ideas regarding testing in Ruby with rspec.
---

# Unit Tests | Ruby

## Warning “circular require considered harmful”

:::{admonition} info
This problem seems to only shows up if you use `ruby -w` to run a given program.
:::

Sometimes while practicing and writing algorithms, we write the unit tests and code in the same file (as we do with the [HtDP](https://htdp.org/) [teaching languages](https://docs.racket-lang.org/htdp-langs/beginner.html#%28form._%28%28lib._lang%2Fhtdp-beginner..rkt%29._check-expect%29%29)).

Something like this:

```rb
#
# add.rb
#

##
# $ gem install rspec 
# $ rspec add.rb
##
require 'rspec'

def add(x, y)
  x + y
end

describe 'add()' do
  it 'should add x and y' do
    expect(add(-1, 1)).to eq 0
  end
end
```

This is OK and works well.
Except if we then try to include the `add.rb` module into another module because you want to reuse the `add()` method, then we might get strange warnings with `ruby -w`:

```rb
#
# sum_arr.rb
#
require 'awesome_print'
require_relative './add'
```

```text
$ ruby -w sum-arr.rb

warning: loading in progress, circular require considered harmful
```

Note that `sum_arr.rb` does nothing but to require the two other files.
In this case, it seems something doesn't go along well between `awesome_print` and `add.rb` importing `rspec`.
Not requiring `awesome_print` in `sum_arr.rb` or not requiring `rspec` in `add.rb` causes the warning to cease.

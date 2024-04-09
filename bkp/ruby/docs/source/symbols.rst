Symbols
=======

Intro
^^^^^

Syntactically, these are all Ruby symbols:

* ``:name``
* ``:btn_add``
* ``:has_many``
* ``:+``

In many cases, Ruby methods, variables, etc. can be represented by a symbol with the same name. For instance, the method ``+`` can be represented by the symbol ``:+``, in the same way, in some situations, the object/variable ``email`` could be represented by the symbol ``:email``.

A first, less elegant implementation of a ``sum_all`` method::

    def sum_all(*numbers)
      total = 0
      for num in numbers
        total += num
      end
      total
    end

    p sum_all()
    # →  0

    p sum_all(5)
    # →  5

    p sum_all(5, 1, 10)
    # →  16

We can achieve the same result as above using a more functional style of programming, using ``Enumerable#reduce`` and passing the ``+`` method in its symbol form to the ``reduce`` method. This second version looks much more elegant::

    def sum_all(*numbers)
      numbers.reduce(&:+)
    end

    p sum_all()
    # →  nil (Oops)

    p sum_all(5)
    # →  5

    p sum_all(5, 1, 10)
    # →  16

Except it returns ``nil`` if we pass zero arguments. That is especially troublesome if we are using the return value to do further maths... Therefore, we should probably be doing this::

    def sum_all(*numbers)
      # Added this line!!!
      return 0 if numbers.length.zero?
      numbers.reduce(&:+)
    end

    p sum_all()
    # →  0 (Now we are talking business!)

    p sum_all(5)
    # →  5

    p sum_all(5, 1, 10)
    # →  16

And *even better*, we can pass ``reduce`` a default value so we don't need the ``if`` modifier to avoid returning ``nil`` when no arguments are passed::

    def sum_all(*numbers)
      # 0 is the default, initial value to add more values to.
      numbers.reduce 0, :+
    end

    sum_all()
    # → 0 (Great!)


Basically, we passed the ``+`` method (in its symbol form) and ``reduce`` used it to sum the values.

TIP: Use ``pry`` to see the docs: ``show-doc Enumerable#reduce``.

Why doesn't it work‽
^^^^^^^^^^^^^^^^^^^^

The docs say we can use a block or a symbol. We used a symbol (``:+``), but we could also have used a block::

    def sum_all(*numbers)
      # `reduce` taking a default vaule of zero and a block
      # to add the numbers.
      numbers.reduce 0 { |acc, n| acc + n }
    end

``nums.reduce(0, &:+)`` converts ``+`` to a block or to a proc?

Why ``nums.reduce(0, :+)`` works, but ````['a', 'b'].map(:upcase)`` does not work and we have to use `&:upcase` instead? Is it because `reduce` can take both a symbol _and_ a block, but `map` can only take a block

excerpt from Enumerable#reduce docs:

Returns a new array with the results of running block once
for every element in enum.

experpt from Enumerable#map docs

Returns a new array with the results of running block once
for every element in enum.

When we pass a block to a method, Ruby converts the block to a proc, and then executes it. Blocks can't be executed. They can only be passed as arguments, but procs can be run, and that is why Ruby converts blocks to procs. When a method ``yeld``'s, then that conversion doesn't happen (TODO: explain this somewhere else or here).

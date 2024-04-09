.. highlight:: ruby
   :linenothreshold: 3

Methods
=======

Basics About Ruby Methods
^^^^^^^^^^^^^^^^^^^^^^^^^

Let's define a method that prints a string, and one that returns a string, and then invoke/call those two methods::

    def say_hello
      # Prints the string.
      puts 'Hello World!'
    end

    def get_hello
      # Returns the String.
      'Hello World!'
    end

    # Don't need to `puts` because the method does it already.
    say_hello()
    # → Hello World!

    # Needs `puts` because the method just return the string:
    puts get_hello()
    # → Hello World!

Method Parenthesis
^^^^^^^^^^^^^^^^^^

Ruby does not require parenthesis to invoke methods. Some times they are
required to avoid ambiguities, which is not the case of the two methods
exemplified above, so, we could just do this instead::

    say_hello
    # → Hello World!

    puts get_hello
    # → Hello World!


Ruby methods enforce arity. If a method is defined with two formal parameters,
it has to be called with exact two arguments::

    def sum(a, b)
      a + b
    end

    sum(5, 2)    # → OK, the exact number of expected args.
    sum(5, 2, 1) # → Error, more args than expected.
    sum(9)       # → Error, less args than expected.

We can also declare methods to declare zero or more arguments. We just have to use the syntax `*args`.::

    def sum_all(*nums)
        nums.reduce(0, :+)
    end


idea
----
Methods that take any number of arguments, using the ``*args`` syntax, and then
operate on args, like, args.length, args.each, etc.

The ``*args`` type of parameter has the lowest priority. It only ever gets any
values after required parameters and parameters with default values get their
dues.

idea
----
Methods that take required *and* ``*rest``, like def sum(a, b, \*rest). a and b are
required, \*rest takes zero or more. sum takes *at least* two args.

idea
----
Arguments with default values::

    def greet(name, msg = 'Hello')
      return "#{msg}, #{name}."
    end


What will it print? ::

    def try_me(a, b, *args, other)
      p a
      p b
      p args.length
      p args
      p other
    end

    try_me(10, 20, 'foo', 'bar')
    # → 10
    # → 20
    # → 1
    # → ['foo']
    # → 'bar'

``*args`` sees that ``other`` wants an argument and respectfully
sponges up only 'foo'.

Calling with::

    try_me(10, 20, 'foo')
    # → 10
    # → 20
    # → 1
    # → ['foo']
    # → 'bar'

``*args`` sees that ``other`` wants an argument and selflessly
refrains itself from sponging up any arguments so that
``other`` can get the value 'foo'.

Now this is an error. ``try_me`` requires *at least* three arguments::

    try_me(10, 20)


idea complex args usage
-----------------------

Let's see a more complex method parameters case study::

    def params_demo(x, y = 10, *args, other)
      p x, y, args, other
    end

Four args, one has a default value, another is the “catch all” sponge argument
(which means it also accepts zero values).  That means means only ``x`` and
``other`` are really required.

Let's start by passing five arguments::

    params_demo 5, 7, 'foo', 'bar', 15
    # → 5
    # → 7
    # → ["foo", "bar"]
    # → 15

Since we passed more than enough arguments, each paramether had some value to
grab onto. ``x`` received ``5``, ``y`` did not need to use its default value of
``10``, and used the argument value of ``7`` instead, ``'foo'`` and ``'bar'``
ended up in ``args`` and ``other`` got ahold of ``15``.

When the ``*sponge`` argument is at the end, it is visually easier to see what
arguments it will be bound to. When it is in some middle position, then it is
trickier.

In summary, the priority when binding values to arguments is: first required
arguments are handled, then the optional ones (those with default values), and
then the “catch all” ``*sponge``-syntax ones.

idea parameter order
--------------------

Required ones are prioritized, no mater whether they appear in the parameter
list.

'Albus', 'Percival', 'Wulfric', 'Brian', 'Dumbledore'

Example::

    def greet(first = 'Albus', *middle, last)
      p first, middle, last
    end
    greet 'Dumbledore'
    # → "Albus"
    # → []
    # → "Dumbledore"

``last`` is the only required argument, and is bound to the value
``'Dumbledore'``. ``first`` retains its default, and ``*middle`` has nothing
to be bound to.

Example::

    greet 'Percival', 'Vulfric', 'Brian', 'Dubmledore'
    "Percival"
    ["Vulfric", "Brian"]
    "Dubmledore"

In this case, since we have more than enough arguments for all of the three
formal parameters, required, optional and sponge arguments, all of them get
some value passed when inoking the method. That means ``first`` did not retain
its default value (because it had something else be be bound to), and the name
of the most honorable and wise wizard ended up incorrect because the first name
'Albus' was lost. This works::

    greet 'Albus', 'Percival', 'Vulfric', 'Brian', 'Dubmledore'
    "Albus"
    ["Percival", "Vulfric", "Brian"]
    "Dubmledore"

idea fullname example
---------------------

Let's look at a method that takes some arguments to compose someone's full
name::

    def fullname(first = 'Unknown', *middle, last)
      name = "#{first} #{middle.join(' ')} #{last}"
      # Removes any potential extra whitespace between words.
      name.gsub(/\s+/, ' ')
    end

If we call with just one argument, it gets bound to ``last``::

    fullname 'Dumbledore'
    # → Unknown Dumbledore

If we call with two arguments, ``first`` binds to the first argument
(ignoring its default value), # and ``last`` binds to the second argument::

    fullname 'Albus', 'Dumbledore'
    # → Albus Dumbledore

If we call call with three or more, ``first`` binds to the first argument
(ignoring its default value), ``middle`` binds to all the other arguments
except the last one, and ``last`` binds to the last argument::

    fullname 'Albus', 'Percival', 'Wulfric', 'Brian', 'Dumbledore'
    # → Albus Percival Wulfric Brian Dumbledore



ruby method return gotchas
^^^^^^^^^^^^^^^^^^^^^^^^^^

Produces the warning “unused literal ignored”.
Returns ‘Hello.’ and the string above it is not even used.

----
def greet
  'This is useless...' # <1>
  'Hello.' # <2>
end
----

Same with this, which uses a heredocument (heredocs are strings)::

    def greet
      <<-LARACROFT
      This heredoc is useless too...
      LARACROFT
      'Hello.'
    end

The __END__.

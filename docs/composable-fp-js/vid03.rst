=====================================================================
03 - Enforce a null check with composable code branching using Either
=====================================================================

Here's the `video lesson`_.

.. _`video lesson`:
   https://egghead.io/lessons/javascript-composable-code-branching-with-either


Intro to Either, Left and Right
-------------------------------

``Either`` is a type that provides two *sub types*:

- ``Right``: for success conditions

- ``Left``: for failure conditions.

The difference between ``Left`` and ``Right`` when compared with
``Box`` is in the way we define ``fold()``. ``Left().map()`` is also
different than ``Box().map()``, as well see in the next few
paragraphs.

We generally don't know beforehand if we have a success or failure
case, and therefore, our fold must account for both. Instead of
receiving one function, like ``Box(v).fold(f)``, both
``Left(v).fold(?, ?)`` and ``Right(v).fold(?, ?)`` receive an error
handling function and a success handling function. Something like
this:

.. code-block:: text

   .fold(errorFn, successFn)

``Left().map()`` is peculiar because it refuses to apply its function
argument to the value. Since we are dealing with some sort of failure,
we can't map over the value. We don't have a “value”, but some sort
of error instead.

.. note::

   It is also common to say *left* and *right* functions to refer to
   the error and success functions:

.. code-block:: text

   .fold(leftFn, rightFn)

Finally, note that for ``Left(value).fold(leftFn, rightFn)`` we don't
use it's second function argument ``rightFn``, and for
``Right(value).fold(leftFn, rightFn)`` we don't use its ``leftFn``. To
avoid problems with linters or TSServer, we can ignore non-used
parameters using the underscore.

.. tip::

   Several programming languages use the underscore ``_`` U+5f LOW
   LINE to indicate that the parameter in that position should be
   ignored.

Using this ``Either`` type we can do *pure functional* error handling,
code branching, null checks and other things that capture the concept
of **disjunction**, that is, the concept of “*or*”.

OK, this is a high level overview of the subject. See the :ref:`v1
impl Left Right` below.


Left and Right Unit Tests
-------------------------

Pay special attention how the unit tests assert that ``Left().map()``
**DOES NOT** apply the provided function to the value, and the value
remains unmodified.

Also, notice that we assert that ``fold()`` applies the *left*
function for ``Left(v).fold(l, r)`` and the *right* function for
``Right(v).fold(l, r)``.

.. literalinclude:: /../src/composable-fp-js/vid03/Either.spec.js
   :language: javascript


.. _v1 impl Left Right:

JSDoc Type Definitions
----------------------

It is nice to have well defined type definitions for these to help us
read and write code, including the awesome TSServer intellisense:

.. image:: /../docs/composable-fp-js.assets/vim-tsserver-jsdoc-typedef-intellisense-1.png
   :alt: Vim TSServer JSDoc TypeDef Intellisense

No, this is not TypeScript. It is vanilla JavaScript! But the
magnificent **TSServer is brilliant enough to provide types from the
JSDoc comments**.

.. literalinclude:: /../src/composable-fp-js/vid03/typedefs.js
   :language: javascript

Implementation of Left and Right
--------------------------------

.. literalinclude:: /../src/composable-fp-js/vid03/Either.js
   :language: javascript


Use of Left and Right
---------------------

The Problem
~~~~~~~~~~~

First of all, the example that attempts to blindly chain method
invocation on values (not using ``Either`` ``Left`` and ``Right`` sub
types):

.. literalinclude:: /../src/composable-fp-js/vid03/vid03a.js
   :language: javascript

The problem is that certain functions (or methods if you prefer) can
only be applied to certain values. ``findColor()`` returns
``undefined`` when it can't find the color by the name provided. But
we are blindly trying to chain ``.slice()`` on the resulting value and
we have no guarantees that it is a value whose prototype provides a
``slice()`` method.

.. code-block:: text

   $ node --interactive

   > ''.slice(5)
   ''

   > 'ECMAScript'.slice(4)
   'Script'

   > [].slice(7)
   []

   > [1, 2, 3].slice(1)
   [ 2, 3 ]

   > ['.js', '.ts', '.rb', '.hs', '.c'].slice(3)
   [ '.hs', '.c' ]

   > (42).slice(1)
   Uncaught TypeError: 42.slice is not a function

   > /regex/.slice(5)
   Uncaught TypeError: /regex/.slice is not a function

   > null.slice(1)
   Uncaught TypeError: Cannot read property 'slice' of null

   > undefined.slice(3)
   Uncaught TypeError: Cannot read property 'slice' of undefined

In our particular case, we can slice on strings and arrays (even empty
ones) without raising an exception, but not on values of some other
types.

.. note::

   This is not an exhaustive list, but just a quick demonstration
   of the problem.


Actually Making Use of Left and Right
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid03/vid03b.js
   :language: javascript

Notice how we changed the implementation of ``findColor()`` to return
an ``Either`` type (which means it will return either a
``LeftContainer`` or a ``RightContainer``).

And because we changed ``findColor()`` to use ``Either``, we now
cannot be blindsided by just chaining ``slice()`` and hoping for the
best. No, we now *map* over the value to apply ``slice()``, and the
code will branch out correctly depending on whether we have a *left*
for a *right* container and things cannot possibly blow up.

.. image:: /../docs/composable-fp-js.assets/ivan-drago-I-cannot-be-defeated.png
   :alt: Ivan Drago - Rocky IV - “I cannot be defeated.”

..
.. For the image, I took a screenshot from this video:
..
.. https://youtu.be/dHY9Hz5rD0s
..
.. and then edited it in Gimp to add the text with the drop shadow.
..


We decided to produce the string ‘No color’ when a color cannot be
returned. We could have taken other approaches, like returning a
default color, an empty string, undefined, etc. We would make that
choice based on what client code would be expected to handle the
result of ``findColor()``.


Improving With fromNullable
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now our ``findColor()`` returns an ``Either``, which is a more
functional style approach and considerably reduces the chance of
exceptions. Sadly, though, it now contains an assignment, which is
what we tried to avoid in our first examples from video 1 on Box.

We then implement ``fromNullable()``, which takes a value wraps it
into an ``Either``, correctly using ``Left`` or ``Right``
appropriately.


.. literalinclude:: /../src/composable-fp-js/vid03/vid03c.js
   :language: javascript


Final Thoughts
~~~~~~~~~~~~~~

It is important to keep in mind that we are now branching out based on
whether we have a value or not. Not having a value means ``undefined``
or ``null`` as per our current implementation. Our ``fromNullable()``
function branches to ``Left()`` in those two cases. Beware: we are not
totally and magically free from problems. We just know that we have a
value or not, but we don't know the type of that value.

.. code-block:: javascript

   function getId(user) {
     return fromNullable(user.id);
   }

   log(
     getId({ id: 103 })
     .map(i => i.split(''))
     .fold(_ => 'Oops', i => i),
   );

The code above results in an exception:

.. code-block:: text

   TypeError: i.split is not a function

Our ``map()`` is the implementation from ``Right()``, which does apply
its callback function to the value. But the value here is a number,
and ``Number.prototype`` does not have a ``split()`` method. **We
still have the responsibility of applying proper functions depending
on the type of values we are dealing with**. So, for example, perhaps
in the example above, we could try to make sure we have a string by
first mapping ``String`` and then doing the splitting:

.. code-block:: javascript

   log(
     getId({ id: 103 })
     .map(String)
     .fold(_ => 'Oops', i => i.split('')),
   );
   // → [ '1', '0', '3' ]


=============================================================
01 - Create linear data flow with container style types (Box)
=============================================================

Here's the `video lesson`_.

.. _`video lesson`:
   https://egghead.io/lessons/javascript-linear-data-flow-with-container-style-types-box

.. note::

   Assume this function is always defined:

   .. code-block:: js

      const l = console.log.bind(console);

Intro
-----

We want a function that given a numeric string like "64", increments
that number and then returns the character that the number
represents. For example, if you ``man ascii`` you'll see that the
decimal number 65 represents the character ‘A’, 66 the character ‘B’
and so on and so forth. So, if we input the string "64", it gets
incremented to the number 65 and then converted to the character ``A``.

As a side note, take a look at what ``String.fromCharCode()`` does:

.. code-block:: js

   String.fromCharCode(65);
   // → 'A'

   for (let i = 97; i <= 102; ++i)
     l(String.fromCharCode(i));
   // → a
   // → b
   // → c
   // → d
   // → e
   // → f

.. tip::

   UTF-8 is backwards-compatible with ASCII. The first 128 UTF-8
   chars precisely match the first 128 ASCII chars. `Read more
   <https://developer.mozilla.org/en-US/docs/Glossary/UTF-8>`_.

The Test Suite
--------------

These cases seem to be enough to cover the domain of our function.

.. literalinclude:: /../src/composable-fp-js/vid01/box.spec.js
   :language: js


A Note on the type 1String
--------------------------

JavaScript doesn't have a type for ``Char``, therefore, we use the
`HtDP 1String type`_:

.. code-block:: js

   //
   // A 1String is a String of length 1, including:
   // • "a"
   // • "Z"
   // • "\\" (the backslash),
   // • " " (the space bar),
   // • "\t" (tab),
   // • "\r" (return), and
   // • "\b" (backspace).
   //
   // INTERP: Represents keys on the keyboard and other 1 character strings.
   //

.. _`HtDP 1String type`:
   https://htdp.org/2021-5-4/Book/part_one.html#%28tech._1string%29


Temporary Variables
-------------------

This first implementation uses temporary variables to store the result
of each step in the process.

.. literalinclude:: /../src/composable-fp-js/vid01/box-v1.js
   :language: js

CONS:

- Too many temporary variables (variables to store intermediary
  results of each step);
- Beginner-like approach and coding style;

See this:  `this file </../src/composable-fp-js/box-v3b.js>`_


Nested Function Invocations
---------------------------

.. literalinclude:: /../src/composable-fp-js/vid01/box-v2.js
   :language: js

CONS:

- Nesting of invocation is harder to read, easy to get lost.
- Not scalable. Hard to add new stuff in anywhere in the chain.


Manual Container
----------------

This example use the concept of a “box” to map over values. We
“manually” add the input value into a one-element array.

.. literalinclude:: /../src/composable-fp-js/vid01/box-v3.js
   :language: js

PROS:

- Easier to read the sequence of things that happen.
- Easy to add new operations any where in the chain.

**(1)**: Note how we use index notation ``[0]`` at the very last thing
in our processing. That is because we introduced the concept of
containers but did not (yet) come up with a way to extract the value
from the container without this manual, indexing approach (we'll
improve on this soon).

Manual Container With Helper Functions
--------------------------------------

All those arrow functions as callbacks to ``.map`` could be extracted,
making the chaining look more neat and clean.

.. literalinclude:: /../src/composable-fp-js/vid01/box-v4.js
   :language: js


Container Type
--------------

.. literalinclude:: /../src/composable-fp-js/vid01/box-v5.js
   :language: js

.. note::

   The ``inspect`` thing used in the video does not work in recent
   versions of node (2021, v14 at least). Overriding ``toString``
   should work. But then we must make sure we try to log the box as a
   string to trigger the ``toString`` mechanism.

This example use the concept of a “box” to map over values. We also
add a ``fold`` function that can also “map” a function over a value,
but instead of returning another Container, it returns the value
itself. ``fold`` should generally be used as the last thing in the
chain.

**(1)**: We use ``fold`` here to finally return the value itself,
*unboxing* it from the container.


``map`` is not supposed to only loop over things. It has to do with
*composition within a context*. ``Box`` is the context in this case.
``Box`` is a “container” type to capture different behaviours.
``Box`` is the **identity functor**.

PROS:

- Easier to read the sequence of things that happen.
- Easy to add new operations anywhere in the chain.
- We can unify method invocations ``s.trim()``, function invocations
  ``parseInt(...)``, operators ``1 + 1``, and qualified invocations
  ``String.fromCharCode()`` (in our case, turned into cleaner helper
  functions).


nextChar example
----------------

TODO: Create an example with a function ``nextChar``, which given a
char like ‘a’ returns ‘b’. Keep it simple and don't bother with the
boundaries of the alphabet.



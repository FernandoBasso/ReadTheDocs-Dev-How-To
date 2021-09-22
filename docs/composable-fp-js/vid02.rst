=====================================================
Refactor Imperative  to Composed Expression using Box
=====================================================

Here's the `video lesson`_.

.. _`video lesson`:
   https://egghead.io/lessons/javascript-refactoring-imperative-code-to-a-single-composed-expression-using-box

Intro
-----

This video is about refactoring three functions that are originally
implemented in a very procedural and imperative style to a more
functional, composed expressions using the Box container type.

Box
---

Let's first extract ``Box`` into its own module and unit test it.

The Unit Tests
~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/lib/box.spec.js
   :language: js

Box Implementation
~~~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/lib/box.js
   :language: js

Our ``Box.toString`` method does do a very good job in stringifying
non-literal values, like arrays and objects. We could get fancy and do
more complex stuff but we do not care much for that. We are interested
in chaining and unboxing the value, and that means ``map`` and ``fold``.

Another important note is that the vid01 only used **unary** functions
while exemplifying the chaining. Take a look at the ``Box`` unit tests
and see that I did some *currying* and *partial application* to work
with ``split`` and ``join``, whose arity is two.


moneyToFloat()
--------------

The Test Suite
~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid02/money-to-float.spec.js
   :language: js

.. note::

   **(1)**, **(2)**, **(3)**: When we produce a number in JavaScript,
   its output is reduced to the simplest possible form. For example
   1.00 becomes 1, and 1.10 becomes 1.1. The ``moneyTofloat`` function
   is about parsing the string to a number. It does not format that
   number so some trailing and leading zeroes are dropped. Careful
   though, leading zeros cause octal interpretation under certain
   circumstances.

   .. code:: js

      Number('017')
      // → 15 (octal interpretation)

      Number('018')
      // → 18 (zero is dropped, decimal interpretation because 8
      // is not part of octal system and the leading zero means
      // nothing in this case)

      Number('0.20')
      // → 0.2 (leading zero is significant here, trailing zero
      // not significant, so it is dropped)

   Again, parsing numbers and printing them in number form is not the
   same as formatting them for UIs.


Imperative Style
~~~~~~~~~~~~~~~~

First, the normal, procedural, imperative style implementation:

.. literalinclude:: /../src/composable-fp-js/vid02/money-to-float-v1.js
   :language: js


Functional Style
~~~~~~~~~~~~~~~~

Using our “container”.

.. literalinclude:: /../src/composable-fp-js/vid02/money-to-float-v2.js
   :language: js

In this case we indeed have more characters of code, but it has
*unnested* the expression and removed all the assignments. It endows
some other advantages as mentioned in the previous lesson.

Note we ``map`` operations over the value inside the container. The
last step usually involves ``fold`` so we apply one last function to
the value while also *unboxing* it so we have access to the value
itself, and not the full container.


==========================================================
02 - Refactor Imperative  to Composed Expression using Box
==========================================================

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

percentToFloat()
----------------

The Unit Tests
~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid02/percent-to-float.spec.js
   :language: js

Imperative Style
~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid02/percent-to-float-v1.js
   :language: js


Instead of all the temp variables, we could implement with nesting of
invocations, like this:

.. code:: js

   function percentToFloat(percentStr) {
     return parseFloat(s.replace(/\%/g, '')) * 0.01;
   }


Functional Style
~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid02/percent-to-float-v2.js
   :language: js

We are not required to pass the value directly to ``Box``. We can do
some pre-processing beforehand, e.g:

.. code-block:: js

   Box(percentStr.replace(/\%/g, '')
     .map(s => Number.parseFloat(s))
     .fold(n => n * 0.01)

We do not always need to use containers, and we are not forced to do
all the operations from the container chaining. We can use our best
judgement according to each situation. It is important to master the
tools and techniques so we can then make informed decisions on how,
where and when to use them.

applyDiscount()
---------------

The Unit Tests
~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid02/apply-discount.spec.js
   :language: js

Imperative Style
~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid02/apply-discount-v1.js
   :language: js

Easy enough so far. Note that ``moneyToFloat()`` and
``percentTofloat`` both return the value, not the container.


Functional Style
~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid02/apply-discount-v2.js
   :language: js

In the video, Brian decides to change ``moneyToFloat()`` and
``percentTofloat`` so they return the container, not the value. In
that case, the ``applyDiscount()`` implementation can be done like
this:

.. code-block:: js

   function moneyToFloat(price, discount) {
     return moneyToFloat(price)
       .fold(cost => percentToFloat(discount)
         .map(savings => cost - cost - savings));
    }

Both ways are OK. Just wanted to show that we don't need to change
existing functions to implement ``applyDiscount()``.


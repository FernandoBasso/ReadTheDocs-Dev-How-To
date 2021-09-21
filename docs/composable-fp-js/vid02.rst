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


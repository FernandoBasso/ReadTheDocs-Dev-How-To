Haskell Sectioning
==================

Intro
-----

Sectioning refers to the property that we can partially apply either the left
or the right side argument of an infix function/operator first.

Basic Syntax
------------
When using sectioning with commutative functions, the side in which the
operator is placed makes no difference because the order of the arguments does
not change the result.

.. code-block::
  λ> (2+) 3
  5
  λ> (+2) 3
  5


But when the function is not commutative, like `(^)`, then it does change the results.

.. code-block::
  λ> (2^) 3
  8
  λ> (^2) 3
  9

``2 ^ 3`` is 8, but ``3 ^ 2`` is 9.

.. code-block::
  λ> (1/) 2
  0.5
  λ> (/1) 2
  2.0

Subtraction vs Negation
-----------------------

To be continued 😅.


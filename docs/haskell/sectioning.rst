Haskell Sectioning
==================

Intro
-----

Infix operators [#]_ by definition have to take two arguments, one on the left, and
one on the right. We can partially apply this operator to either the left hand
side or the right hand side operand first. We call this *sectioning*.

With commutative functions, it doesn't matter which argument is applied first,
but it makes all the difference with non-commutative functions.

.. container:: qanda

   Intro text!

   .. container:: question

      What is the answer to this question‽

   .. container:: answer

      This is the answer:

      .. code-block:: bash

         $ git log
         $ man tree

      This is the text that follows the answer 😄

   And this ends the entire QandA. May the source...

question 2
----------

.. container:: qanda

   .. container:: question

      Let's see?

   .. container:: answer

      The code is:

      .. code-block:: bash

         #!/usr/bin/env bash
         title='The Script'
         printf 'The title is “%s”.\n'
         echo -e $'the end\n'

   Did it work?

   Yes!!!

Top level paragraph.


The End.


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


.. rubric:: Footnotes

.. [#] Remember that operators are functions, like any other function. We
   call them operators because they are defined with symbols like ``+`` or
   ``^``, etc. instead of alphabethic names like ``sum`` or ```power``.


=========================================
Find Int That Appears Odd Number of Times
=========================================

- Challenge_

.. _Challenge:
   https://www.codewars.com/kata/54da5a58ea159efa38000836

Approach 1
----------

1. Let *curX* be the first element of the list.

2. Count how many times *curX* appear in the list.

   1. If the result is odd, return *curX*.

   2. If the result is even or zero.

   3. Let *curX* be the next element of the list.

3. Repeat steps 2 to 4, getting the next element of the
   list each time (elem 3, elem 4, etc.).

4. If reach the end of the list without getting an odd
   count, then return false.


Scheme
------

The Test Suit
~~~~~~~~~~~~~

.. literalinclude:: /../src/codewars/fundamentals/scm/int-odd-times/find-n.spec.scm
   :language: scheme

v1 single function
~~~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/codewars/fundamentals/scm/int-odd-times/find-n-v1.scm
   :language: scheme

v2 smaller, composable functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unit Tests for the Helper Functions
...................................

.. literalinclude:: /../src/codewars/fundamentals/scm/int-odd-times/find-n-v2-helpers.spec.scm
   :language: scheme

Implementation using the composed functions
...........................................

.. literalinclude:: /../src/codewars/fundamentals/scm/int-odd-times/find-n-v2.scm
   :language: scheme

In this case, we have a *curried* ``eqx?`` function that we partially
apply inside ``oddx?`` to help filter out and then count the number of
times the current ``x`` appear in the list.

v3 car cdr
~~~~~~~~~~

This implementation is a courtesy of `Mario Domenech Goulart`_. It
does not use ``find`` and ``filter`` and instead makes use of ``car``,
``cdr`` and ``null?``. It does not need any help of external libraries
or SRFIs. It is probably the most idiomatic example in terms of
techniques used, like the *loop pattern*, for instance.

.. _`Mario Domenech Goulart`:
   http://parenteses.org/mario/


Unit Tests for the Helper Functions
...................................

.. literalinclude:: /../src/codewars/fundamentals/scm/int-odd-times/find-n-v3-helpers.spec.scm
   :language: scheme

Implementation using car, cdr and null?
.......................................

.. literalinclude:: /../src/codewars/fundamentals/scm/int-odd-times/find-n-v3.scm
   :language: scheme


=========================================
Find Int That Appears Odd Number of Times
=========================================

- Challenge_

.. _Challenge:
   https://www.codewars.com/kata/54da5a58ea159efa38000836


.. _int-odd-times-approach-1-search-and-count:

Approach 1 (search and count)
-----------------------------

1. Let *curX* be the first element of the list.

2. Count how many times *curX* appear in the list.

   1. If the result is odd, return *curX*.

   2. If the result is even or zero.

   3. Let *curX* be the next element of the list.

3. Repeat steps 2 to 4, getting the next element of the
   list each time (elem 3, elem 4, etc.).

4. If reach the end of the list without getting an odd
   count, then return false.

.. _int-odd-times-approach-2-bitwise-xor:

Approach 2 Bitwise XOR
----------------------

.. note::

   This approach only works, and works precisely because we are told
   there is always one — and always a single one — integer that appears
   an odd number of times. They state that:

       "There will always be only one integer that appears an odd number
       of times."

   It is implied by that sentence **and by the example test cases**
   that there is never more than one integer that appears an odd
   number of times.

XORing :math:`x` to itself an even number of times always yields
:math:`0`.

.. code-block:: text

   1 ^ 1 = 0
   1 ^ 1 ^ 1 ^ 1 = 0


XORing :math:`x` to :math:`0` yields :math:`x`.

.. code-block:: text

   1 ^ 0 = 1
   5 ^ 0 = 5


If we XOR :math:`x` an even number of times, the result is always
:math:`0`. It means an even number of :math:`x`'s cancel themselves.

For this challenge, since it is ASSUMED that there will be only one
number that appears an even number of times (and there will always be
one), we can use this bitwise XOR technique to obtain the answer.

.. code-block:: text

   1 ^ 1 ^ 3 = 3
   1 ^ 2 ^ 1 ^ 2 ^ 3 = 3


The XOR operator has the associative and commutative properties. Thus,
the order of the numbers in the array doesn't matter for this case.

https://en.wikipedia.org/wiki/Exclusive_or#Properties


TypeScript
----------

The original function in Codewars has a return type of simply
`number`. I changed this to `undefined | number` to satisfy `find`
even though the exercise says we can assume there will always be
single number that appears an odd number of times.

Unit Tests
~~~~~~~~~~

The unit tests should work for all approaches and solutions. Changing
the implementation should not produce different results.

.. literalinclude:: /../src/codewars/typescript/6kyu/int-odd-times/find-n.test.ts
   :language: typescript


Solution 1 (single function, approach 1)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using :ref:`approach 1<int-odd-times-approach-1-search-and-count>`,
one single function that takes care of the entire logic. Not bad.

.. literalinclude:: /../src/codewars/typescript/6kyu/int-odd-times/find-n-v1.ts
   :language: typescript


Solution 2 (helper functions, approach 1)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Also using :ref:`approach
1<int-odd-times-approach-1-search-and-count>`, we have the main
``findN`` function which in turn makes use of specialized helper
functions. Looks overkill for this case but the helper functions could
be useful and used in other contexts as well, though. Therefore it is
worth the example.

.. literalinclude:: /../src/codewars/typescript/6kyu/int-odd-times/find-n-v2.ts
   :language: typescript

Solution 3 (bitwise XOR)
~~~~~~~~~~~~~~~~~~~~~~~~

The clever solution with :ref:`approach
2<int-odd-times-approach-2-bitwise-xor>` using bitwise
XOR! I did not come up with this solution myself, but instead saw it
from other people's solution. What I did was to write an explanation
on how it works.

.. literalinclude:: /../src/codewars/typescript/6kyu/int-odd-times/find-n-v3.ts
   :language: typescript


Scheme
------

The Test Suit
~~~~~~~~~~~~~

.. literalinclude:: /../src/codewars/scheme/6kyu/int-odd-times/find-n.spec.scm
   :language: scheme

v1 single function
~~~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/codewars/scheme/6kyu/int-odd-times/find-n-v1.scm
   :language: scheme

v2 smaller, composable functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unit Tests for the Helper Functions
...................................

.. literalinclude:: /../src/codewars/scheme/6kyu/int-odd-times/find-n-v2-helpers.spec.scm
   :language: scheme

Implementation using the composed functions
...........................................

.. literalinclude:: /../src/codewars/scheme/6kyu/int-odd-times/find-n-v2.scm
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

.. literalinclude:: /../src/codewars/scheme/6kyu/int-odd-times/find-n-v3-helpers.spec.scm
   :language: scheme

Implementation using car, cdr and null?
.......................................

.. literalinclude:: /../src/codewars/scheme/6kyu/int-odd-times/find-n-v3.scm
   :language: scheme


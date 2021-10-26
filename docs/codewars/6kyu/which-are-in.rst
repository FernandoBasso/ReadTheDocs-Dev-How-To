============
Which Are In
============

- Challenge_

.. _Challenge:
   https://www.codewars.com/kata/550554fd08b86f84fe000a58


Approach 1
----------

Let's call *needle* the substrings and *haystack* the array where to
search for the *needle*.

1. Let *needle* be the first substring and *matches* be an initial,
   empty array.

2. Search for *substring* in the *haystack*.

   1. If found, add *needle* to the list of *matches* to be returned.

3. If there is no more *needle* to process, return *matches*.

4. Let *needle* be the next substring.

5. Go back and continue from step 2.


JavaScript
----------

The Test Suite
~~~~~~~~~~~~~~

.. literalinclude:: /../src/codewars/6kyu/which-are-in/js/which-are-in.spec.js
   :language: javascript

Solution 1
~~~~~~~~~~

.. literalinclude:: /../src/codewars/6kyu/which-are-in/js/which-are-in-v1.js
   :language: javascript

Scheme
------

The Test Suite
~~~~~~~~~~~~~~

.. literalinclude:: /../src/codewars/6kyu/which-are-in/scm/which-are-in.spec.scm
   :language: scheme

Solution 1
~~~~~~~~~~

.. literalinclude:: /../src/codewars/6kyu/which-are-in/scm/which-are-in-v1.scm
   :language: scheme



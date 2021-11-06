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

.. literalinclude:: /../src/codewars/javascript/6kyu/which-are-in/which-are-in.spec.js
   :language: javascript

Solution 1
~~~~~~~~~~

.. literalinclude:: /../src/codewars/javascript/6kyu/which-are-in/which-are-in-v1.js
   :language: javascript

TypeScript
----------

The Test Suite
~~~~~~~~~~~~~~

.. literalinclude:: /../src/codewars/typescript/6kyu/which-are-in/which-are-in.test.ts
   :language: typescript

Solution 1
~~~~~~~~~~

.. literalinclude:: /../src/codewars/typescript/6kyu/which-are-in/which-are-in-v1.ts
   :language: typescript


Scheme
------

The Test Suite
~~~~~~~~~~~~~~

.. literalinclude:: /../src/codewars/scheme/6kyu/which-are-in/which-are-in.spec.scm
   :language: scheme

Solution 1
~~~~~~~~~~

.. literalinclude:: /../src/codewars/scheme/6kyu/which-are-in/which-are-in-v1.scm
   :language: scheme



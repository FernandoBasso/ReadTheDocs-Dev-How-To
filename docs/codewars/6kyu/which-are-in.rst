=========================================
Which Are In
=========================================

- Challenge_

.. _Challenge:
   https://www.codewars.com/kata/550554fd08b86f84fe000a58

The names ``a1`` and ``a1`` in this exercise are ill-chosen. It would
be better to name them like “haystack” (the list of strings to search
in) and ``substrs`` (the list of substrings to be searched into each
string in the haystack). My solution uses these names.

Also, the parameters should have their order swapped. If we wanted to
curry this thing, it would be more idiomatic and useful to have the
haystack as the first parameter. I did not do it because then it
wouldn't pass the Codewars unit tests.


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

The Test Suit
~~~~~~~~~~~~~

.. literalinclude:: /../src/codewars/6kyu/which-are-in/js/which-are-in.spec.js
   :language: javascript

Solution 1
~~~~~~~~~~

.. literalinclude:: /../src/codewars/6kyu/which-are-in/js/which-are-in-v1.js
   :language: javascript


=============
Friend or Foe
=============

- Challenge_

.. _Challenge:
   https://www.codewars.com/kata/55b42574ff091733d900002f

The name of the function in the Codewars challenge is ill-chosen. The
function returns a list of friends (even if an empty one). The name of
the function should imply some pluralism. It could be ``onlyFriends``
or something similar (not ``friends`` because functions should be
like verbs, indicating some sort of action, not nouns, indicating some
sort of things.

Approach 1
----------

1. Let *friends* be an empty list.

2. If input list of names is empty, return *friends*.

3. Let *name* be the left-most, not yet checked name of the list.

4. If *name* length is 4 (four), then *name* is your friend. Append it
   to the *friends* list.

5. Go back to step 2.

JavaScript
----------

Unit Tests
~~~~~~~~~~

.. literalinclude:: /../src/codewars/javascript/6kyu/friend-foe/friend-foe.spec.js
   :language: javascript

Solution 1
~~~~~~~~~~

.. literalinclude:: /../src/codewars/javascript/6kyu/friend-foe/friend-foe-v1.js
   :language: javascript

TypeScript
----------

Unit Tests
~~~~~~~~~~

.. literalinclude:: /../src/codewars/typescript/6kyu/friend-foe/friend-foe.test.ts
   :language: typescript

Solution 1
~~~~~~~~~~

.. literalinclude:: /../src/codewars/typescript/6kyu/friend-foe/friend-foe-v1.ts
   :language: typescript


Scheme
------

Unit Tests
~~~~~~~~~~





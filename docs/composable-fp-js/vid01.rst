========================================================
Create linear data flow with container style types (Box)
========================================================

Here's the `video lesson`_.

.. _`video lesson`:
   https://egghead.io/lessons/javascript-linear-data-flow-with-container-style-types-box

.. note::

   Assume this function is always defined:

   .. code-block:: js

      const l = console.log.bind(console);

Intro
-----

We want a function that given a numeric string like "64", increments
that number and then returns the character that the number
represents. For example, if you ``man ascii`` you'll see that the
decimal number 65 represents the character ‘A’, 66 the character ‘B’
and so on and so forth. So, if we input the string "64", it gets
incremented to the number 65 and then converted to the character ``A``.

As a side note, take a look at what ``String.fromCharCode()`` does:

.. code-block:: js

   String.fromCharCode(65);
   // → 'A'

   for (let i = 97; i <= 102; ++i)
     l(String.fromCharCode(i));
   // → a
   // → b
   // → c
   // → d
   // → e
   // → f

.. tip::

   UTF-8 is backwards-compatible with ASCII. The first 128 UTF-8
   chars precisely match the first 128 ASCII chars. `Read more
   <https://developer.mozilla.org/en-US/docs/Glossary/UTF-8>`_.

The Test Suite
--------------

These cases seem to be enough to cover the domain of our function.

.. literalinclude:: /../src/composable-fp-js/vid01/box.spec.js
   :language: js
   :linenos:


A Note on the type 1String
--------------------------

JavaScript doesn't have a type for ``Char``, therefore, we use the
`HtDP 1String type`_:

.. code-block:: js

   //
   // A 1String is a String of length 1, including:
   // • "a"
   // • "Z"
   // • "\\" (the backslash),
   // • " " (the space bar),
   // • "\t" (tab),
   // • "\r" (return), and
   // • "\b" (backspace).
   //
   // INTERP: Represents keys on the keyboard and other 1 character strings.
   //

.. _`HtDP 1String type`:
   https://htdp.org/2021-5-4/Book/part_one.html#%28tech._1string%29


Temporary Variables
-------------------

This first implementation uses temporary variables to store the result
of each step in the process.

.. code-block:: js

   /**
    * Produces the next char based on the numeric input string.
    *
    * @example
    * nextCharFromNumStr('64');
    * // → 'A'
    *
    * @example
    * nextCharFromNumStr(' 64  ');
    * // → 'A'
    *
    * ASSUME: The input is a valid numeric value.
    *
    * @param {string} value The numeric string.
    * @return {string} The computed character.
    */
   const nextCharFromNumStr = value => {
     const trimmed = value.trim();
     const num = parseInt(trimmed, 10);
     const nextNum = num + 1;
     return String.fromCharCode(nextNum);
   };

   l(nextCharFromNumStr(' 64'));
   l(nextCharFromNumStr(' 96  '));
   // → A
   // → a

CONS:

- Too many temporary variables (variables to store intermediary
  results of each step);
- Beginner-like approach and coding style;

See this:  `this file </../src/composable-fp-js/box-v3b.js>`_


Nested Function Invocations
---------------------------

.. code-block:: js

   /**
    * Produces the next char based on the numeric input string.
    *
    * ASSUME: The input is a valid numeric value.
    *
    * @param {string} value The numeric string.
    * @return {string} The computed character.
    *
    * @sig String -> String
    *
    * @example
    * nextCharFromNumStr('64');
    * // → 'A'
    *
    * @example
    * nextCharFromNumStr(' 64  ');
    * // → 'A'
    */
   const nextCharFromNumStr = value => {
     return String.fromCharCode(parseInt(value.trim(), 10) + 1);
   };


CONS:

- Nesting of invocation is harder to read, easy to get lost.
- Not scalable. Hard to add new stuff in anywhere in the chain.

.. code-block:: js

   l(nextCharFromNumStr('64'));
   l(nextCharFromNumStr('96'));
   // → A
   // → a


Manual Container
----------------

This example use the concept of a “box” to map over values. We
“manually” add the input value into a one-element array.

.. code:: js

   /**
    * Produces the next char based on the numeric input string.
    *
    * ASSUME: The input is a valid numeric value.
    *
    * @param {string} value The numeric string.
    * @return {string} The computed character.
    *
    * @example
    * nextCharFromNumStr('64');
    * // → 'A'
    *
    * @example
    * nextCharFromNumStr(' 64  ');
    * // → 'A'
    */
   const nextCharFromNumStr = value => {
     return [value].map(s => s.trim())
                   .map(s => parseInt(s, 10))
                   .map(i => i + 1)
                   .map(i => String.fromCharCode(i));
   };


PROS:

- Easier to read the sequence of things that happen.
- Easy to add new operaions any where in the chain.

.. code-block:: js

   l(nextCharFromNumStr('64'));
   l(nextCharFromNumStr('96'));
   // → ['A']
   // → ['a']


Container Type
--------------

.. code:: js

   /**
    * A Value consumed and produced by the Container `map's function.
    *
    * @typedef {any} Value
    */

   /**
    * @typedef {Object} Container
    * @property {function(function(Value): Value): Container} map Map a
    *   function over the Value.
    * @property {function(): string} toString Our custom stringification
    *   of the object.
    */

   /**
    * Creates a chainable container.
    *
    * @param {Value} val
    * @return {Container}
    */
   const Box = val => {
     return {
       map: f => Box(f(val)),
       toString: () => `Box(${val})`,
     };
   };

   /**
    * Produces the next char based on the numeric input string.
    *
    * @example
    * nextCharFromNumStr('64');
    * // → 'A'
    *
    * @example
    * nextCharFromNumStr(' 64  ');
    * // → 'A'
    *
    * ASSUME: The input is a valid numeric value.
    *
    * @param {string} value The numeric string.
    * @return {string} The computed character.
    */
   const nextCharFromNumStr = value => {
     return Box(value)
       .map(s => s.trim())
       .map(s => parseInt(s, 10))
       .map(i => i + 1)
       .map(i => String.fromCharCode(i))
       .map(c => c.toLowerCase());
   };

   const result1 = nextCharFromNumStr(' 64 ');
   const result2 = nextCharFromNumStr(' 96 ');

   l(result1 + '');
   l(String(result2));
   // → Box(a)
   // → Box(a)

.. note::

   The ``inspect`` thing used in the video doesn't work in recent
   versions of node (2021, v14 at least). Overriding `toString' should
   work. But then we must make sure we try to log the box as a string
   to trigger the `toString' mechanism.

This example use the concept of a “box” to map over values.

PROS:

- Easier to read the sequence of things that happen.
- Easy to add new operations anywhere in the chain.
- We can unify method invocations `s.trim()', function invocations
  `parseInt(...)', operators  `1 + 1', and qualified invocations
  `String.fromCharCode()'


Unboxing Value
--------------

Currently, our container return the value inside the container. We may
also want to get the value itself. To do that, we add a ``fold``
function that also maps over a value, but instead of returning the
value inside the container, it returns the value by itself.

.. code:: js

   /**
    * A Value consumed and produced by the Container `map`s function.
    *
    * @typedef {any} Value
    */

   /**
    * @typedef {Object} Container
    * @property {function(function(Value): Value): Container} map Map a
    *   function over the Value.
    * @property {function(): string} toString Our custom stringification
    *   of the object.
    */

   /**
    * Creates a chainable container.
    *
    * @param {Value} val
    * @return {Container}
    */
   const Box = val => {
     return {
       map: f => Box(f(val)),
       fold: f => f(val),
       toString: () => `Box(${val})`,
     };
   };

   /**
    * Produces the next char based on the numeric input string.
    *
    * ASSUME: The input is a valid numeric value.
    *
    * @param {string} value The numeric string.
    * @return {string} The computed character.
    *
    * @example
    * nextCharFromNumStr('64');
    * // → 'A'
    *
    * @example
    * nextCharFromNumStr(' 64  ');
    * // → 'A'
    */
   const nextCharFromNumStr = value => {
     return Box(value)
       .map(s => s.trim())
       .map(s => parseInt(s, 10))
       .map(i => i + 1)
       .map(i => String.fromCharCode(i))
       .fold(c => c.toLowerCase());
   };

   l(nextCharFromNumStr(' 64 '));
   l(nextCharFromNumStr(' 96 '));
   // → a
   // → a

``map`` is not supposed to only loop over things. It has to do with
*composition within a context*. ``Box`` is the context in this case.
``Box`` is a “container” type to capture different behaviours.
``Box`` is the **identity functor**.


nextChar example
----------------

TODO: Create an example with a function ``nextChar``, which given a
char like ‘a’ returns ‘b’. Keep it simple and don't bother with the
boundaries of the alphabet.



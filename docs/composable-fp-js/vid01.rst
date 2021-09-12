========================================================
Create linear data flow with container style types (Box)
========================================================

Here's the `link to the video lesson`_.

.. _`link to the video lesson`:
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


.. tip::

   UTF-8 is backwards-compatible with ASCII. The first 128 UTF-8
   chars precisely match the first 128 ASCII chars. `Read more
   <https://developer.mozilla.org/en-US/docs/Glossary/UTF-8>`_.

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

Version 1
---------

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

   //
   // CONS:
   // • Too many temporary variables.
   // • Very prodecural.
   //

   l(nextCharFromNumStr(' 64'));
   l(nextCharFromNumStr(' 96  '));
   // → A
   // → a

   //
   // vim: set tw=72:
   //



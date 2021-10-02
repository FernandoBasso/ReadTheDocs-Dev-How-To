============================================
Search and Replace Until Next UpperCase Char
============================================

We have this JSdoc comment:

.. code-block:: text

   /**
    * @typedef {function(function(Value): Value): LeftContainer} LeftMapFn
    */


We want to replace “Left” with “Right”. There are several ways we
could do it. One of them is to *change until the next uppercase
character*.

The cursor is under the “@” character::

   @typedef {function(function(Value): Value): LeftContainer} LeftMapFn
   ^

In NORMAL MODE, we type this sequence: :kbd:`f` :kbd:`L`. Now, the
cursor is under “L”::

    ...Value): LeftContainer} LeftMapFn
               ^

Then, start the editing proper with this sequence: :kbd:`c` :kbd:`/`
`\\` :kbd:`u` :kbd:`Enter`. Now you have::

   ... Value): Container} LeftContainer
               ^

- ``c`` is the command for *change*

- ``/`` starts a forward search

- ``\u`` means “match an uppercase character”

- ``<Enter>`` finally executes the entire command.

Note that that occurrence of “Left” is no more, and that you are in
INSERT MODE. Type “Right” then :kbd:`Esc` to go into NORMAL MODE. At
this point we have this::

   ... Value): RightContainer} LeftContainer
                   ^

Now we type :kbd:`;` do repeat the last :kbd:`f` :kbd:`L` search and
the cursor gets positioned on the next “L” from “LeftMapFn”::

   ... Value): RightContainer} LeftContainer
                               ^

We finally type :kbd:`.` to repeat the last modification, which means
it replaces “Left” with “Right”. Our final result is this::

   /**
    * @typedef {function(function(Value): Value): RightContainer} RightMapFn
    */

Some Thoughts
-------------

Had we used :kbd:`c` :kbd:`t` :kbd:`L` to also search for the next uppercase,
then we wouldn't be able to use :kbd:`;` to repeat the search for the
next “L”, but worse, we would not be able to repeat the change from
“Left” to “Right” with simply typing :kbd:`.`.


TL;DR command
-------------

So, we have this text::

    @typedef {function(function(Value): Value): LeftContainer} LeftMapFn
    ^

With the cursor under “@”, he entire sequence goes like this (the
spaces are just to improve readability, not to be actually typed)::

   fL c/\u Right <Esc> ; .

We end up with this text::

   @typedef {function(function(Value): Value): RightContainer} RightMapFn
                                                                   ^

References
----------

From Vim built-in help:

- ``:help 04.2``

- ``:help \\u``

- ``:help ;``

- ``:help .``

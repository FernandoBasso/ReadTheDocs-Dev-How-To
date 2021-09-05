===============
echo and printf
===============


echo newline
------------

.. code:: shell-session

   $ PS1='\$ '
   $ echo -n

Why does ``echo -n`` still produce a newline and the next prompt is
on a line of its own?

It is not ``echo`` that is producing a newline. Because we first add
a newline by hitting :kbd:`Enter` (a.k.a :kbd:`Return`) in order to
execute the command, then bash prints nothing (we provided nothing for
``echo`` to print), the output is a newline from our :kbd:`Enter` and
nothing else. The ``$`` prompt is positioned on a line of its own
simply because ``echo`` had nothing to print.

When we do ``echo -n foo`` and hit :kbd:`Enter`, we first produce a
newline, then bash prints 'foo'.

.. code-block:: shell-session

   $ PS1='\$ '
   $ echo -n foo<Return>
   foo$

Then the prompt ``$`` is positioned immediately after 'foo'. After
all, we asked ``echo`` NOT to append a newline, so, ``echo`` prints
'foo' and the prompt is positioned right after ``echo``'s output.







How to print `-n'?
------------------

If we just do ``echo -n``, the ``-n`` is treated as the ``-n`` option
(do not append a newline).

This doesn’t work:

.. code:: shell-session

   $ echo -- -n
   -- -n.

Not what we want…​ Bash’s ``echo`` honors `the
specs <https://pubs.opengroup.org/onlinepubs/9699919799/utilities/echo.html>`__:

   The echo utility shall not recognize the “\-\-” argument in the manner
   specified by Guideline 10 of XBD Utility Syntax Guidelines; “\-\-” shall
   be recognized as a string operand.

   — echo POSIX spec

We can ``man ascii`` and look for the numeric value of ``\-\-``:

**Excerpt from \`man ascii’.**

.. code:: text

   Oct   Dec   Hex   Char
   ──────────────────────
   ...
   055   45    2D    -
   ...

Then we can use the ``-e`` option for ``echo`` and use the octal or
hexadecimal values to produce ``-`` and just implicitly concatenate both
``-`` and ``n``.

.. code:: shell-session

   $ echo -e '\055'n
   -n

   $ echo -e '\x2d'n
   -n

It has been said that:

   "Any fool can make something complicated. It takes a genius to make
   it simple.”

Therefore:

.. code:: shell-session

   $ echo -n -; echo n;

Jokes apart, the version with ``-e`` and ``\x2d`` is cool and useful
too. It is nice to have the tools and know how to use them.

Nice question and discussion:
`When and how was the double-dash (\-\-)
introduced as an end of options delimiter in Unix/Linux?
<https://unix.stackexchange.com/questions/147143/when-and-how-was-the-double-dash-introduced-as-an-end-of-options-delimiter>`__

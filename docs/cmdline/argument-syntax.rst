=====================
Shell Argument Syntax
=====================

There are a few, but **extremely important** concepts that we must
keep in mind to make reasonable use of the command line and write
shell scripts in general.

Space
-----

In the shell, whitespace matters. It is used to break the input into
tokens. See `the spec`_ and `bash shell syntax documentation`_.

.. _`the spec`:
   https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_03

.. _`bash shell syntax documentation`:
   https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Syntax

For the shell, the first *word* (a.k.a *token*) is the command, or the
name of the program to run, and the remaining *words* are parameters
to be passed to the program.

.. code-block:: shell-session

   $ printf '%d\n' {1..3}
   1
   2
   3

Here, ``printf`` is the first token (the name of the program to be
run), ``%d\n`` is the second token (quotes are removed - unless
escaped - before the argument is passed to the program, and ``{1..3}``
is the third token, except the shell (Bash in my case) performs brace
expansion before passing the results as individual tokens to the
``printf`` program. When the shell finds the newline, it then executes
the command line.

It is paramount that we prevent the shell from word splitting in
certain cases.

.. code:: shell-session

   $ ls -1
   message.txt
   secret message.txt
   secret.txt

Now we want to remove ``secret message.txt``:

.. code:: shell-session

   $ ls -1
   message.txt
   secret message.txt
   secret.txt

   $ rm -v secret message.txt
   rm: cannot remove 'secret': No such file or directory
   removed 'message.txt'

   $ ls -1
   'secret message.txt'
   secret.txt

Oh shoot! Because we did not prevent the shell from breaking ``secret
message.txt`` into individual tokens, what was passed to ``rm`` was
not a single parameter, but two: ``secret`` and ``message.txt``.
``rm`` was unable to remove a file named ``secret`` because no such
file exists (we have ``secret.txt``) but **was able to** remove
``message.txt`` because that was a file that really existed (but no
longer). Unfortunately, we did not remove ``secret message.txt`` which
is what we wanted, **but accidentally removed** ``secret.txt`` **which
we didn't want to**. Yes, that is a sad story...

We must prevent the shell from performing word splitting in cases like
this (and many others). What we should have done is this:

.. code:: shell-session

   $ rm -v 'secret message.txt'
   removed 'secret message.txt'

   $ ls -1
   message.txt
   secret.txt

Now we removed ``secret message.txt`` and not incidents took place.


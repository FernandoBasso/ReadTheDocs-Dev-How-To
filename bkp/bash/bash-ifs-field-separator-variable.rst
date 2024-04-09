IFS Field Separator Default Value
=================================

First, let’s debug the default value of ``IFS``:

.. code:: shell-session

   $ echo -n "$IFS" | hexdump -C
   00000000  20 09 0a
   00000003

   $ printf '%s\n' "$IFS" | od -c -a -x
   0000000      \t  \n  \n
            sp  ht  nl  nl
              0920    0a0a

So, the default value of ``IFS`` is a space, a horizontal tab, and a
newline (which are natural word separators). Remember the excerpt from
`bash’s man
page <https://www.gnu.org/software/bash/manual/bash.html#Arrays>`__.

Using IFS to Convert Array To String
====================================

IFS can be used to our advantage to convert arrays to strings in a very
simple and clean manner. But first, remember this:

   If the word is double-quoted, ${name[*]} expands to a single word
   with the value of each array member separated by the first character
   of the IFS variable.

   — bash manual

Note it reads **first character of the IFS variable**! So, yeah, the
``IFS`` variable can contain more than one character which could
potentially be used as a word separator.

Let’s play with IFS. First, create an array:

.. code:: shell-session

   $ nums=(1 2 3 4)

The first char of ``IFS`` is the space, so, as we quote ``nums`` and use
the ``[*]`` subscript, the output is a string with the elements of
``nums`` separated by a space.

.. code:: shell-session

   $ printf '“%s”\n' "${nums[*]}"
   “1 2 3 4”

We can change the value of ``IFS``, and then using the ``[*]`` syntax
helps to better understand the whole mater:

.. code:: shell-session

   $ IFS='#'; printf '“%s”\n' "${nums[*]}"
   “1#2#3#4”

   $ IFS='_'; printf '“%s”\n' "${nums[*]}"
   “1_2_3_4”

   $ IFS=$'\t'; printf '“%s”\n' "${nums[*]}"
   “1      2       3       4”

   $ IFS=$'\n'; printf '“%s”\n' "${nums[*]}"
   “1
   2
   3
   4”

Looks like setting ``IFS`` for a single command does not work:

.. code:: shell-session

   $ nums=(1 2 3 4)

   $ IFS=- echo "${nums[*]}"
   2 2 3 4

   $ IFS=- printf '%s\n' "${nums[*]}"
   1 2 3 4

In neither of the cases is ``-`` used with the syntax ``[*]``. Both
``echo`` and ``printf`` are shell built-ins:

.. code:: shell-session

   $ type echo printf
   echo is a shell builtin
   printf is a shell builtin

But it works for ``read`` (which is also a built-in):

.. code:: shell-session

   $ IFS=- read -r -a arr <<<'x-y-z'

   $ echo "${#arr[@]}"
   3

   $ printf '%s\n' "${arr[@]}"
   x
   y
   z

Yeah, ``read`` honored ``-`` as the field separator, and ``arr`` was
assigned three elements, ``a``, ``b`` and ``c``.

@TODO: Why does it work for ``read`` but not for ``echo`` and
``printf``? Could it simply be that some commands simply do not honor
IFS? In any case, ``help read`` explicitly tells us that IFS is used
with ``read``.

@TODO: Check whether the man pages for echo and printf, as well as their
posix specs say something about IFS.

IFS Links and Resources
-----------------------

-  https://unix.stackexchange.com/questions/120575/understanding-the-default-value-of-ifs

-  https://unix.stackexchange.com/questions/26784/understanding-ifs

-  https://unix.stackexchange.com/questions/92187/setting-ifs-for-a-single-statement

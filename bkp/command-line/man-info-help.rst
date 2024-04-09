.. __man_info_and_help:

Man, Info and Help
==================

Always read man pages, help pages, and info pages.

.. tip::

   If you are new to \*nix, don’t expect to understand very much of the
   manuals when reading and trying stuff in the very first few attempts.
   It will really depend a lot on your background and previous
   experience, skills and expertise. Just keep trying.

.. __man:

man
---

.. code:: shell-session

   $ man man
   $ man --help
   $ man foo

When someone tells you something like “it is docummented in some_command
(3)”, they mean it is in section 3 of the man pages. Then you would do
``man 1 some_command`` or ``man some_command.3``:

A real example of that could be with the printf(1) command or printf(3)
from the C Standard Library:

.. code:: shell-session

   $ man 1 printf
   $ man printf.1

   $ man 3 printf
   $ man printf.3

If we don’t know what a man page name would be, we can search the man
page names and their sort descriptions by using ``-k``, which accepts a
regular expression. By the way, ``man -k pattern`` is the same as
``apropos pattern``.

.. tip::

   If ``apropos`` or ``man -k`` says “nothing appropriate”, read
   ``man mandb`` and then run ``mandb`` as root.

.. code:: shell-session

   $ man -k bc
   ... produces to many results...

So, let’s match commands that start with “bc”:

.. container:: formalpara-title

   **man -k ^bc on Ubuntu 19.04**

.. code:: shell-session

   $ man -k ^bc
   bc (1)               - An arbitrary precision calculator language
   bccmd (1)            - Utility for the CSR BCCMD interface
   bcmp (3)             - compare byte sequences
   bcopy (3)            - copy byte sequence

.. container:: formalpara-title

   **man -k ^bc on Arch Linux as of September, 2019**

.. code:: shell-session

   $ man -k ^bc
   BC (3x)              - direct curses interface to the terminfo capability dat...
   bc (1)               - An arbitrary precision calculator language
   bc (1p)              - arbitrary-precision arithmetic language
   bcmp (3)             - compare byte sequences
   bcomps (1)           - biconnected components filter for graphs
   bcopy (3)            - copy byte sequence

.. note::

   On Ubuntu, *bc (1p)* wasn’t available, but it was on Arch Linux.

.. note::

   A “p” right after a section number of a man page means the standard
   POSIX program/behavior. “bc (1p)” refers to the POSIX specs and
   behavior, while

Section numbers are more or less standard across Unix-like OSes, but the
letters may vary.

.. __finding_info_node_names:

Finding Info Node Names
~~~~~~~~~~~~~~~~~~~~~~~

.. code:: shell-session

   $ info sed --output - | grep '^\*\s.\+::'
   * Introduction::               Introduction
   * Invoking sed::               Invocation
   * sed scripts::                'sed' scripts
   * sed addresses::              Addresses: selecting lines
   * sed regular expressions::    Regular expressions: selecting text
   * advanced sed::               Advanced 'sed': cycles and buffers
   * Examples::                   Some sample scripts
   * Limitations::                Limitations and (non-)limitations of GNU 'sed'
   * Other Resources::            Other resources for learning about 'sed'
   * Reporting Bugs::             Reporting bugs
   * GNU Free Documentation License:: Copying and sharing this manual
   * Concept Index::              A menu with all the topics in this manual.
   * Command and Option Index::   A menu with all 'sed' commands and

   $ info sed 'sed scripts' --output - | grep '^\*\s.\+::'
   * sed script overview::      'sed' script overview
   * sed commands list::        'sed' commands summary
   * The "s" Command::          'sed''s Swiss Army Knife
   * Common Commands::          Often used commands
   * Other Commands::           Less frequently used commands
   * Programming Commands::     Commands for 'sed' gurus
   * Extended Commands::        Commands specific of GNU 'sed'
   * Multiple commands syntax:: Extension for easier scripting

   $ info sed 'sed scripts' 'the "s" command' --output - | vim -

   $ info sed 'sed scripts' 'the "s" command'

Or commands that end with “print” (but not “printf”, for example):

.. code:: shell-session

   $ man -k print$
   FcFontSetPrint (3)   - Print a set of patterns to stdout
   FcPatternPrint (3)   - Print a pattern for debugging
   FcValuePrint (3)     - Print a value to stdout
   isprint (3)          - character classification functions
   iswprint (3)         - test for printing wide character
   print (1)            - execute programs via entries in the mailcap file

Bear in mind that all of these do the same thing:

.. code:: bash

   man -k some_command
   man --apropos some_command
   apropos some_command

To search on the entire text of the man pages, use:

.. code:: bash

   man --global-apropos some_command
   man -K some_command

Note that it is an uppercase “K” this time.

.. __help:

help
----

Bash has builtin commands, like ``help``, ``pwd``, ``type``, and ``cd``.

.. code:: shell-session

   $ type cd
   cd is a shell builtin

``help`` is a builtin command that gives help on other builtin commands.

.. tip::

   To see the list of Bash’s builtin commands, simply type ``help``
   without any arguments.

And to see the help for a builtin, you can either do man bash and search
for that command or do:

.. code:: shell-session

   $ help <some builtin command>

   $ help help
   $ help builtin
   $ help .
   $ help source
   $ help command
   $ help [
   $ help [[
   $ help echo
   $ help printf
   # etc...

.. __info:

info
----

First:

.. code:: shell-session

   $ info info
   $ man info
   $ info --help

.. note::

   Of course, on Arch Linux, we are fine, but on Ubuntu, we need to
   ``apt install texinfo-doc-nonfree``.

Generally, info pages are more user-friendly and tutorial-like than man
pages. To read info pages, do

.. code:: shell-session

   $ info <program or command>

   $ info ed
   $ info sed
   $ info bash

.. note::

   Not all programs and commands have info pages, and when an info page
   does not exist for a given command, ``info`` shows the man page
   instead. ``vi``, ``vim`` and ``rsync`` are some such programs.

You can also open info directly into a section of an info document (if
you know the name of the section), something like:

.. code:: shell-session

   $ info sed 'execution cycle'

And programs in the *coreutils* group have an *invocation* section:

.. code:: shell-session

   $ info coreutils

   $ info '(coreutils) echo invocation'
   $ info coreutils 'echo invocation'

   $ info '(coreutils) printf invocation'
   $ info coreutils 'printf invocation'

   $ info '(coreutils) kill invocation'
   $ info coreutils 'kill invocation'

TODO: How to discover the names of the info sections for a given
program? For example “info sed, then sed scripts, then the s command”.

From GNU Emacs, you can read the info pages with:

.. code:: text

   C-h i m <command>

   # For example:
   C-h i m sed

Info has a lot of nomenclature, concepts and commands. ``info info``
explains about commands to find stuff inside info, navigate documents,
etc. It is a somewhat complex system. Yet, a powerful one.

.. __info_summary:

info summary
~~~~~~~~~~~~

.. code:: text

   info emacs --node Files
   info '(emacs)Files'

   info /usr/local/share/info/bash.info
   info ~/docs/doc.info

   info sed 'sed scripts' 'the "s" command'
   info emacs 'user input'

Run ``info info 'moving the cursor'``.

.. note::

   ``META`` (or ``ALT``) can also be achieved by hitting ``ESC``. Like
   ``ESC-f`` for ``forward-word``. And ``ESC`` itself can be produced
   with ``C-[``. ``DEL`` is ``Backspace``.

For a quick glance at *all* info commands and key bindings, open any
info page, and press ``C-h``.

.. __cp_man_page_example:

\`cp' Man Page Example
----------------------

``man cp`` produces this:

.. container:: formalpara-title

   **Exerpt of \`man cp' on Arch Linux as of 2019**

.. code:: text

   CP(1)                            User Commands                           CP(1)

   NAME
          cp - copy files and directories

   SYNOPSIS
          cp [OPTION]... [-T] SOURCE DEST
          cp [OPTION]... SOURCE... DIRECTORY
          cp [OPTION]... -t DIRECTORY SOURCE...

   DESCRIPTION
          Copy SOURCE to DEST, or multiple SOURCE(s) to DIRECTORY.

          Mandatory  arguments  to  long  options are mandatory for short options
          too.

Let’s understand the man page syntax.

“cp” is the name of the command or program. No mistery.

Anything inside “[” and “]” means that thing is optional. In this case,
``[OPTION]`` means that command line options are optional, that is, you
can do something like ``cp -v foo.txt foo.txt.bpk``, where ``-v`` is an
*option*, or simply ``cp foo.txt foo.txt.bpk``, and not use ``-v`` or
any other option at all. You can think as options as flags the enable,
disable, or configure the way the program should behave.

The three dots, ``…​``, like in ``[OPTION]…​`` or ``SOURCE…​``, means
that thing may occur more than one time. If something is optional, it
may occur zero or more times. If that thing is required, then it has to
occur one or more times. So, in the case of:

.. code:: text

   cp [OPTION]... SOURCE... DIRECTORY

it means we must use ``cp``, followed by zero or more command line
options. Then, ``SOURCE…​`` is required, but it can occur more than
once. Finally, ``DIRECTORY`` is required, and must occur only once.

Recap:

-  ``[THING]`` optional and may occur at most once.

-  ``[THING]...`` optional and may occur zero or more times.

-  ``THING`` required and must occur exactly once.

-  ``THING...`` required and must occur one or more time.

Since ``cp`` accepts multiple sources, we could copy more than one file
at a time to a given destination directory. As an example, let’s copy
three files to a backup directory.

.. code:: shell-session

   $ cp main.c lib.h lib.c ~/bkpdir/

Suppose we want to use the options ``--verbose`` and ``--interactive``
(or their short versions, ``-v`` and ``-i``), we can do:

.. code:: shell-session

   $ cp --verbose --interactive main.c lib.h lib.c ~/bpkdir/

And with the short option syntax, we can group options. All three
commands below do the same thing:

.. code:: shell-session

   $ cp --verbose --interactive foo.txt foo.txt.bpk
   $ cp -v -i foo.txt foo.txt.bpk
   $ cp -vi foo.txt foo.txt.bpk

Note the ``-vi`` instead of ``-v -i`` in the last one!

.. __csi_help_example:

\`csi' -help Example
--------------------

One of the popular Scheme interpreters (repl) is “Chicken”, and its
command line tools include ``csi`` (Chicken Scheme Interpreter, for the
command line repl) and ``csc`` (Chicken Scheme Compiler).

.. note::

   On some distros, the names are now ``chicken-csi`` and
   ``chicken-scs`` because there were conflicts with Mono’s C Sharp
   Compiler and Chicken Scheme Compiler. See this `Mono
   issue <https://github.com/mono/mono/issues/9056>`__, this `Debian bug
   report <https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=509367>`__,
   and this `Arch Linux bug
   report <https://bugs.archlinux.org/task/54040>`__.

.. note::

   ``csi -help`` as of 2019 produces output different than showed here
   (as it was in 2017), but the examples and explanations are still very
   useful and enlightening.

.. code:: shell-session

   $ csi -help

   usage: csi [FILENAME | OPTION ...]

Note that we have the square braces enclosing two things, and there is a
“|” (the pipe character) between those two things. That character means
'OR', that is, either one thing, or the or the other. It doesn’t mean
“invoke csi followed by a filename followed by an option.” Nope, that is
incorrect. What that means is either one of these:

.. code:: shell-session

   $ csi program.scm

   # or
   $ sci <some option>

   # but this is INCORRECT:
   $ sci program.scm <some option>

On the other hand, if you look at the ``csi`` man page (or
``sci -help``), you’ll see that some options require a file name, like
the ``-s`` (or ``-script``) option.

The moral is that the man page shows something that can be easily
misunderstood:

.. code:: text

   csi [FILENAME | OPTION ...]

Can lead one to think the syntax is:

.. code:: shell-session

   $ sci program.scm -s

which is incorrect. The correct is either:

.. code:: shell-session

   $ sci program.scm

or (because the option ``-s`` takes a filename)

.. code:: shell-session

   $ sci -s program.scm

That is, ``csi filename`` or ``csi <option>``, just that some options
require a filename **after** the option itself.

.. __command_options:

Command Options
---------------

Most commands (or programs) accept both long versions and short versions
of options. For example, ``rsync`` has ``-a``, short for ``--archive``,
and ``-r``, short for ``--recursive``, among many others.

Still, even for programs that support both short and long versions of
options, some options my be available only in long form (either because
there was no appropriate single letter left, or for some other,
sometimes odd, reason). For example, ``ls`` has the long option
``--group-directories-first``, and there is no short name for that
option. However, some programs allow the abbreviation of a long option
as long it does not clash with some other option. For instance ``ls``
has only one long option that starts with ``--g`` (which is
``--group-directories-first``), and it allows one to abbreviate it to
something like ``--group-directories``, or ``--group-d``, or even
``--group`` or ``--g``.

To give another example, the program ``xclip`` also allows unambiguous
abbreviations; one can either write ``xclip -selection clipboard`` or
abbreviate to ``xclip -sel clip``. Many other commands allow this sort
of abbreviation.

Another thing to consider is the number of hyphens. For most commands,
short options use one hyphen, and long versions use two. You write
either ``-r`` (one hyphen) or ``--recursive`` (two hyphens). However,
some commands have long options (and sometimes *only* long options, and
behold, they take only *one single hyphen*. ``xclip``, ``chicken-csi``
and ``chicken-csi`` are examples of programs in which the long version
uses only a single hyphen (and allow the unambiguous abbreviations).

Yet others, like ``tar``, do not require the hyphen for the short
versions. That is, you can either do ``tar -cf dir.tar dir/`` or drop
the hyphen and do ``tar cf dir.tar dir/``.

``java`` and ``javac``, has long options, and some use one single
hyphen, like ``-classpath``, while others use two hyphens, like
``--class-path``.

.. __posix_and_gnu:

POSIX and GNU
-------------

POSIX is a standard (specification) defined by the `Open
Group <https://pubs.opengroup.org/onlinepubs/9699919799/>`__. There are
four main sections in the spec:

-  `Base
   Definitions <https://pubs.opengroup.org/onlinepubs/9699919799/idx/xbd.html>`__

-  `System
   Interfaces <https://pubs.opengroup.org/onlinepubs/9699919799/idx/xsh.html>`__

-  `Shell &
   Utilities <https://pubs.opengroup.org/onlinepubs/9699919799/idx/xcu.html>`__
   (this is the one most useful for command line users and
   practictioners)

-  `Rationale <https://pubs.opengroup.org/onlinepubs/9699919799/idx/xrat.html>`__

GNU programs and commands attempt to follow POSIX, but adds several
additional features and “extensions” to standard POSIX. So, when you use
a command line program, it is very likely that you are not using plain,
standard POSIX, but extra features not defined in POSIX as well.

Bash itself can be started with environment variable ``POSIXLY_CORRECT``
set (or with the ``--posix`` option) so it will behave like a real,
plain, bare POSIX shell as much as possible.

In ``sed``, we can read its info page with ``info sed``. In the section
“Sed Scripts > The "s" Command”, we can read this:

.. container:: formalpara-title

   **Excerpt from GNU Sed Info Page**

.. code:: text

   Finally, as a GNU 'sed' extension, you can include a special sequence
   made of a backslash and one of the letters 'L', 'l', 'U', 'u', or 'E'.
   The meaning is as follows:

   '\L'
        Turn the replacement to lowercase until a '\U' or '\E' is found,

   '\l'
        Turn the next character to lowercase,

   '\U'
        Turn the replacement to uppercase until a '\L' or '\E' is found,

   '\u'
        Turn the next character to uppercase,

   '\E'
        Stop case conversion started by '\L' or '\U'.

Most (if not all) GNU command line programs docs explicitly state when
something is not plain POSIX, but an additional GNU feature. We can
assume that most man and info pages are explicit when an option or
something else is not POSIX-compliant or POSIX-defined.

.. __documentation_relationships:

Documentation Relationships
---------------------------

Also worth noting is that some docs refer to some other docs. If a man,
help or info page mentions some other docs, pay attention to it. It
usually means it implements things mentioned in the other docs, and
possibily *extends* and overrides things from the mentioned docs. Let’s
discuss one such example.

If you read the help for the builtin ``printf`` command, it says:

.. container:: formalpara-title

   **Excerpt of bash’s \`help printf'**

.. code:: text

   In addition to the standard format specifications described in printf(1),
   printf interprets:

And then you do ``man 1 printf``, and see:

.. container:: formalpara-title

   **Excerpt of \`man 1 printf':**

.. code:: text

   NOTE:  your shell may have its own version of printf, which usually su‐
   persedes the version described here.  Please refer to your shell's doc‐
   umentation for details about the options it supports.

So, Bash’s printf uses the format especifications defined in printf(1),
but nonetheless, printf(1) tells us that the Shell’s printf “usually
supersedes” *this printf*. Moreover, ``man 1 printf`` talks about C
printf.

If we read `POSIX printf
specs <https://pubs.opengroup.org/onlinepubs/9699919799/utilities/printf.html>`__,
we see it mentions `XBD File Format
Notation <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap05.html>`__,
which says:

.. code:: text

   If the format is exhausted while arguments remain, the excess arguments shall
   be ignored.

So, one would expect that ``printf '%s\n' foo bar`` would print "foo\n"
and ignore "bar", still, take a look at what really happens:

.. code:: shell-session

   $ printf '%s\n' foo bar
   foo
   bar

It is still printing “bar” even though the POSIX spec tells that it
should be ignored. Except that `XCU Command and
Utilities <https://pubs.opengroup.org/onlinepubs/9699919799/utilities/printf.html>`__
extends and superseds `XBD File Format
Notation <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap05.html>`__.
Look:

.. code:: text

   The format operand shall be used as the format string described in XBD File
   Format Notation with the following exceptions:

   ...

   9. The format operand shall be reused as often as necessary to satisfy the
   argument operands.

   ...

So, even though XBD tells that “excess arguments shall be ignored”, XCU
printf overrides that and tells that it *shall be reused to satisfy the
operands*.

.. __end_of_options_echo_example:

End of Options echo Example
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unix shells and programs interpret ``--`` to mean “end of options”.
Guideline 10 on `XBD Utility Syntax Guidelines
10 <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html>`__
says:

.. container:: formalpara-title

   **Excerpt of XBD Utility Syntax Guidelines**

.. code:: text

   The first -- argument that is not an option-argument should be accepted as a
   delimiter indicating the end of options. Any following arguments should be
   treated as operands, even if they begin with the '-' character.

Take a look:

.. code:: shell-session

   $ printf -v
   -bash: printf: -v: option requires an argument
   printf: usage: printf [-v var] format [arguments]

But if we use ``--``, then printf simply prints “-v”:

.. code:: shell-session

   $ printf -- -v
   -v

Then we try it with echo:

.. code:: shell-session

   $ echo -- -e
   -- -e

Oops! echo printed ``-- -e``, not just ``-e``. It seems echo does not
take ``--`` to mean “end of options”. If we run ``help echo``, it says
nothing about ``--``. Then we read `XCU echo spec
page <https://pubs.opengroup.org/onlinepubs/9699919799/utilities/echo.html>`__,
and come accross this:

.. container:: formalpara-title

   **Excerpt of XCU echo spec page**

.. code:: text

   The echo utility shall not recognize the "--" argument in the manner
   specified by Guideline 10 of XBD Utility Syntax Guidelines; "--" shall be
   recognized as a string operand.

So that is it. Since GNU Bash echo does not override the way ``--``
should work according to the specs, it is not even documented in
``help echo``. And we should assume, at least when it comes to ``--``,
that echo bash builtin follows the specs!

.. __other_links_and_resources:

Other Links and Resources
-------------------------

https://wiki.gentoo.org/wiki/Man_page/Navigate

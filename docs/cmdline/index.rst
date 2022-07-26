============
Command Line
============

.. toctree::
   :hidden:
   :maxdepth: 6
   :caption: Command Line:

   help-info-man
   built-in
   argument-syntax
   echo-printf
   bash-brace-expansion
   bash-parameter-expansion
   bash-aliases
   bash-arrays
   terminal/index
   tar.md
   find.md
   hackerrank-shell

Intro
-----

.. image:: ./index.assets/bash-help-1.png
   :alt: Bash Help Command Line

Command line tools have NOT faded out into the past. They have kept
consistently evolving and being improved. They still do! A lot of GUI
interfaces use command line tools behind the scenes.

There exists a specification to standardize how command line utilities
should behave so that they work in a cross-platform, interoperable way.

GNU and BSD command line utilities generally try to follow the specs
but add the so called *extensions* which add further capabilities to
the tools, making writing scripts and programs easier and sometimes
less verbose, but also reduce the portability. Some tools can be run
in a more restrict way that disallow extensions. The docs for each
tool should specify those things. Information about these things is
also scattered across this website's notes and examples when they are
deemed important and/or worthwhile.

Besides reading the specs, also take a look at the sidebar about
reading and understanding man pages, info pages and help pages.

.. note::

   Assume all text and examples on this site is written considering
   GNU tools and the Bash shell unless otherwise noted.

Single Unix Specification (SUS)
-------------------------------

- `XBD: Base Definitions`_
- `XSH: System Interfaces and XBD Headers`_
- `XCU: Shell and Utilities (Commands and Utilities)`_

More info on the link `The Open Group Base Specifications Issue 7,
2018 edition`_. There is also a question about the `difference between
SUS and Open Group`_.

.. _`The Open Group Base Specifications Issue 7, 2018 edition`:
   https://pubs.opengroup.org/onlinepubs/9699919799

.. _`XBD: Base Definitions`:
   https://pubs.opengroup.org/onlinepubs/9699919799/idx/xbd.html

.. _`XSH: System Interfaces and XBD Headers`:
   https://pubs.opengroup.org/onlinepubs/9699919799/idx/xsh.html

.. _`XCU: Shell and Utilities (Commands and Utilities)`:
   https://pubs.opengroup.org/onlinepubs/9699919799/idx/xcu.html

.. _`difference between SUS and Open Group`:
   https://unix.stackexchange.com/questions/14368/difference-between-posix-single-unix-specification-and-open-group-base-specifi/14369


Yes, That Website Sucks
~~~~~~~~~~~~~~~~~~~~~~~

Yes, that website sucks. Not the contents of the website, but the way
it works!

The whole thing is full of iframes. URLs don't change (only the
iframes contents do) when we click on sidebar links which means it is
impossible to share or bookmark different URLs do. HOWEVER, if we
**open links on a new tab** then we get out of the iframe hell
thing. So, open links on a new tab when you need the URL for that
link.


GNU Coreutils
-------------

.. note::

   The examples on this site assume GNU tools and GNU Coreutils.
   If you find something that doesn't work on your version/vendor
   of the tool, feel free to open a PR to this project proposing
   an alternative working solution.

It is worth taking a look at following resources:

- `GNU Coreutils Home Page`_
- `GNU Coreutils FAQ`_
- `GNU Coreutils Documentation`_
- `GNU Coreutils Decoded`_

.. _`GNU Coreutils FAQ`:
   https://www.gnu.org/software/coreutils/faq/coreutils-faq.html

.. _`GNU Coreutils Home Page`:
   https://www.gnu.org/software/coreutils/

.. _`GNU Coreutils Documentation`:
   https://www.gnu.org/software/coreutils/manual/

.. _`GNU Coreutils Decoded`:
   http://www.maizure.org/projects/decoded-gnu-coreutils/

There is also the mind-blowing, awesome `Decoded GNU Coreutils`_
project by MaiZure_.

.. _`Decoded GNU Coreutils`:
 http://www.maizure.org/projects/decoded-gnu-coreutils/

.. _MaiZure:
   http://www.maizure.org/projects/faq.html

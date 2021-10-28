===================
Codewars Challenges
===================

My solutions to the Codewars challenges implemented in a few languages
that interest me. I always do full TDD in everything I code, and these
are no exceptions. Therefore, unit tests are provided as well.

Chicken Scheme
--------------

Install Necessary Eggs
~~~~~~~~~~~~~~~~~~~~~~

For Scheme I use Chicken. Install these eggs:

.. code-block::

   $ chicken-install \
       test \
       srfi-1 \

Running Tests
~~~~~~~~~~~~~

To run the tests, make sure you are inside the directory that contains
the file with the tests, for example:

.. code-block:: text

   $ ls -1
   add.spec.scm
   add.scm

Then, run this command

.. code-block:: text

   $ csi -quiet -batch

   -- testing add ----------------------------------------------------
   should add zeroes ......................................... [ PASS]
   should add positive numbers ............................... [ PASS]
   should add negabive numbers ............................... [ PASS]
   3 tests completed in 0.0 seconds.
   3 out of 3 (100%) tests passed.
   -- done testing add -----------------------------------------------

If running from Emacs + Geiser, make sure you execute ``run-geiser``
*after* you visit the ``.scm`` file you want to run, so that Geiser is
in the directory of that file and the ``(load "..."``) directive does
not fail saying it cannot find the files to load.

To run the tests from Emacs/Geiser, visit the spec file, start Geiser
with ``M-x geiser`` and then do a ``C-c C-b``
(``geiser-eval-buffer``).

TypeScript
----------

I use Deno_ to test and run the TypeScript solutions. Also be sure to
check :ref:`these notes about Deno and absolute imports plus editor
setup <deno project root absolute imports>` on this very site.

.. _Deno: https://deno.land/

Running Tests With Deno
~~~~~~~~~~~~~~~~~~~~~~~

Change to the root directory of where these codewars solutions have
been cloned, e.g:

.. code-block:: text

   $ cd /path/to/devhowto/src/codewars

The run all tests, or specific tests:

.. code-block:: text

   $ deno test 6kyu
   $ deno test 6kyu/friend-foe
   $ deno test 6kyu/friend-foe/friend-foe.spec

Check extra options with ``deno test --help | less``.

.. toctree::
   :maxdepth: 6
   :caption: Codewars Challenges Solved

   6kyu/index


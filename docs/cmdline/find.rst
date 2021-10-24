=============
find examples
=============

Find files and store in array
-----------------------------

.. code::

   $ mapfile -d $'\0' mds < <(find . -iname '*.md' -print0)




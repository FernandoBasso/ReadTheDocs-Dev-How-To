===================
Emacs Org-Mode Tips
===================

RET indents
-----------

When typing a heading ``** foo``:kbd:`Return` the cursor is positioned on the
third column of the next line. If we are at column 0 after such a
heading, :kbd:`Return` also produces the indented next line:

.. code::

   ** fooRET
      ^

Or:

.. code::

   ** foo
   RET
      ^

In both cases, the cursor gets positioned under “f”. The solution is
easy: replace :kbd:`Return` by :kbd:`Ctrl+j`. More on this `Org-mode
mailing list thread on indent on Enter`_.

.. _`Org-mode mailing list thread on indent on Enter`:
   https://orgmode.org/list/40f31dee9bd62d8ca1a071448f554722@isnotmyreal.name/t/#u



================================
Emacs Practical Editing Examples
================================

Searching the buffer for...
---------------------------

Let's see some examples of searching the buffer for certain stuff. Read along!

the word under point
--------------------

You have the point in the first char of a word. Hitthing ``C-s C-w``
searches the buffer for that word.

If point is on other char of the word, then the search happens from
the partial part of the word, from the point's position until the end
of the word. That is, if you have the word ``Makefile`` and the cursor
is on ``f``, then ``C-s C-w`` searches for ``file`` (not ``Makefile``).


string under point
------------------

This is a case for where you want to search for multiple words. One
practical case for this is in reStructuredText where hyperlinks can
have the form::

  Take a look at the `detailed tool X docs`_ for more examples.

  .. _`detailed tool X docs`: http://example.com

That is, one can definee a hyperlink elsewhere and reference it where
needed. See this `screencast <https://youtu.be/Nz76jSfkd8M>`_ a visual
exmaple on how it works and looks like in Emacs.

Just as a side note, here's how the aforementioned reST hyperlink renders:

Take a look at the `detailed tool X docs`_ for more examples.

.. _`detailed tool X docs`: http://example.com


References
----------

- https://stackoverflow.com/questions/202803/searching-for-marked-selected-text-in-emacs


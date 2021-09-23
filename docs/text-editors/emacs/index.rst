=====
Emacs
=====

.. toctree::
   :hidden:
   :maxdepth: 6
   :caption: Emacs

   practical-examples
   org-mode-tips

Installing on macOS
-------------------

emacs-plus
~~~~~~~~~~

With brew, we can either install the default ``emacs`` formula or the
more featureful `homebrew-emacs-plus`_ formula, which has more enabled
features, such as support computing image size, which is in turn useful
for org-mode so we can use things like ``#+ATTR_ORG: :width 600`` for
instance.

.. _homebrew-emacs-plus:
   https://github.com/d12frosted/homebrew-emacs-plus

.. code:: shell-session

   $ brew tap d12frosted/emacs-plus
   $ brew install emacs-plus


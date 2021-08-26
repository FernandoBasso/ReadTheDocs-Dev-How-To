================================
More Examples of Commit Messages
================================

---------------------------
Document what some CSS does
---------------------------

This is a commit that has to do with CSS styling and layout. The
author provides a visual example on how it looks before and after the
commit. The ASCII representation of the new UI is a nice touch and
provides a very clear picture (no pun intended) of results of applying
the commit.


.. code-block::

   dev-how-to [devel *% u=]
   $ git log -1
   commit 86513e14fc97c124340053a471fa82400fb62119 (HEAD -> devel, gl/devel)
   Author: Fernando Basso <redacted email>
   Date:   Thu Aug 26 07:49:30 2021 -0300

   style: Add borders and text around QandaA containers

   Currently, there is no visual clue for the users as to where both the
   question and answer start and end. This commit adds some borders and
   text labels around the question and the answer to help visually identify
   what part is the question and what part is the answer.

   Before this commit, the question and answer look like this:

       How to see what is in gl/devel but not in HEAD?

       Easy enough: 😄

       $ git log HEAD..gl/devel

    And this is how it should like after this commit:

                             +----------+
       +---------------------| question |-+
       |                     +----------+ |
       | How to see what is in gl/devel   |
       | but not in HEAD?                 |
       +----------------------------------+
                               +--------+
       +-----------------------| answer |-+
       |                       +--------+ |
       | Easy enough: 😄                  |
       |                                  |
       |   $ git log HEAD..gl/devel       |
       +----------------------------------+

.. BEWARE that there are some emojis above that emacs is not
   displaying. Let's be careful not to remove then.

.. note::

   It is not that hard to draw some ASCII boxes in vim with
   ``virtualedit``.

   In vim:

   .. code-block::

      :help 'virtualedit'
      :set virtualedit=all


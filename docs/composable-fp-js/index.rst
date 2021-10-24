================================
Composable Functional JavaScript
================================

Intro
-----

Notes and solutions for the `Composable Functional JavaScript`_
free course on Egghead by the most excellent Brian Lonsdorf (a.k.a
DrBoolean).

Related material from the same author is the `Mostly Adequate Guide to
Functional Programming`_. This Dev How To project/site has a `section
with solutions and extra notes`_ on that (awesome and free)
book. Check it out!

My notes and examples sometimes use a few helper functions that either
I created or that I extracted from a video lesson into a reusable
function. If you are reading a page and it seems there is no
definition for a certain function, check the :doc:`helpers` page.

.. _`Composable Functional JavaScript`:
   https://egghead.io/courses/professor-frisby-introduces-composable-functional-javascript

.. _`Mostly Adequate Guide to Functional Programming`:
   https://github.com/MostlyAdequate/mostly-adequate-guide

.. _`section with solutions and extra notes`:
   /magfpjs


A Note on Extra Examples and Deviations From The Videos
-------------------------------------------------------

While studying this content myself and creating these notes and the
unit tests, I sometimes change the original code a little bit, or add
extra notes on “flaws” in his code and propose a different, “better”
solution.

Or course Brian Lonsdorf (the author of the original content and the
videos) knows about all these things. He probably just wanted to focus
on the main ideas, without digressions and to keep the code short and
easy to read.

Since we are writing notes and explanations from the videos in text on
a web page, adding unit tests and sometimes showing different
alternatives, we are OK with adding additional explanations on
tangential topics, like in this case, in which we add extra, further
information on basic JavaScript.

One such example of this happening is on :ref:`Video 05, concatUniq()
imperative style <vid05-concatUniq-imperative-style>`.


A Note on the Wording Used
--------------------------

The explanations (my explantions and notes on the content of the
videos) at times use wording like “beginner-like approach”, or “this
implementation is too much procedural”, or “too much object-oriented”,
etc.

The intent is not to deride beginners [#]_ or to imply that other
programming paradigms are bad [#]_. The focus here is on functional
programming. We strive for a functional style as much as possible
since the goal is to learn and practice this specific programming
paradigm. So, when something does not conform to the principles of FP,
we occasionally make such comparisons and comments as a means to show
how much the given implementation is non-conformant, or how far away
it is to the functional paradigm.

Also, if beginners tend to do things in a certain way, that is OK. The
goal is to keep learning and improving. If the procedural, or the
object-oriented, or functional programming style have such and such
pros and cons, that is OK too [#]_. Again, the goal is to learn so we
can offer more educated opinions and make more informed decisions in
any given situation [#]_.

.. [#] I myself am a beginner in a lot of stuff, have always been, and
       will always be.

.. [#] I myself like to study and practice C, Shell Script, C++, Ruby,
       Java, etc., which are more procedural or object-oriented in style,
       among, of course, Haskell, Scheme, Lisp and similar which are
       either purely functional or are at least feel very comfortable and
       work well for a functional style.

.. [#] Similar things could be said about static-typing (at
       compile time) or dynamic typing (at runtime). Both have
       advantages and disadvantages (that is stating the obvious), but
       worth keeping in mind.

.. [#] I am amazed by John Carmack's (traditionally a C/C++
       programmer) humility talking about his willingness to learn
       Lisp and Functional Programming. He is also humble when he says
       thinks like “I wanted to do some real project in Haskell”
       so he could “talk about their merits on a concrete
       sense rather than an abstract sense” and that he “does *think*
       he *soft of* gets Lisp now.” Watch this `part about Haskell`_
       and this `part about Lisp`_. They are from John Carmack's
       keynote at Quakecon 2013.

.. _`part about Haskell`: https://youtu.be/1PhArSujR_A?t=283

.. _`part about Lisp`: https://youtu.be/1PhArSujR_A?t=799


.. toctree::
   :maxdepth: 6
   :caption: Composable Functional JavaScript

   vid01
   vid02
   vid03
   vid04
   vid05
   helpers

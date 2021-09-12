Data Types Questions and Answers
=================================


types aka
---------

.. container:: qanda

   .. container:: question

      Types are also known as...

   .. container:: answer

      *datatypes*.


enumerate types
---------------

.. container:: qanda

   .. container:: question

      List three datatypes that come with Prelude.

   .. container:: answer

      * ``Char``
      * ``Word``
      * ``Double``

values and types
----------------

.. container:: qanda

   .. container:: question

      Values have types. In Haskell, we cannot have an *untyped* value.  Types
      are like groups that contains values that belong to that group.

      Enumerate a few values that would belong to the type (a.k.a *datatype*)
      ``Char`` and a few that would belong to the type ``Word``.

   .. container:: answer

      * ``Char``:

        * ``'a'``
        * ``'9'``
        * ``' '``

      * ``Word``

        * ``0``
        * ``42``

It is important to note that ``Word`` includes only natural numbers,
from zero onward.


data declarations
-----------------

.. container:: qanda

   .. container:: question

      What do *data declarations* do?

   .. container:: answer

      *Data declarations* define types (a.k.a. *data types*).


basic data declaration
----------------------

.. container:: qanda

   .. container:: question

      .. code-block::

         λ> :info Bool
         data Bool = False | True

      Identify type constructors and data constructors above.

   .. container:: answer

      ``Bool`` is the *type constructor*. ``False`` and ``True`` are the two
      possible *data constructors* that belong to the *type* or *datatype*
      `Bool`.

      .. note::

         Although we say *data declaration*, we are not defining __only__
         *data constructors*. We are in fact defining both *type constructors*
         **and** *data constructors*.



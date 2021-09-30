=====================================================================
03 - Enforce a null check with composable code branching using Either
=====================================================================

Intro to Either, Left and Right
-------------------------------

``Either`` is a type that provides two *sub types*:

- ``Right``: for success conditions

- ``Left``: for failure conditions.

The difference between ``Left`` and ``Right`` when compared with
``Box`` is in the way we define ``fold()``. ``Left().map()`` is also
different than ``Box().map()``.

We generally don't know beforehand if we have a success or failure
case, and therefore, our fold must account for both. Instead of
receiving one function, like ``Box(v).fold(f)``, both
``Left(v).fold(?, ?)`` and ``Right(v).fold(?, ?)`` receive an error
handling function and a success handling function. Something like
this:

.. code-block:: text

   .fold(errorFn, successFn)

``Left().map()`` is peculiar because it refuses to apply its function
argument to the value. Since we are dealing with some sort of failure,
we can't map over the value. We don't have a “value”, but some sort
of error instead.

It is also common to say *left* and *right* functions to refer to
error and success functions:

.. code-block:: text

   .fold(leftFn, rightFn)

Finally, note that for ``Left(value).fold(leftFn, rightFn)`` we don't
use it's second function argument ``rightFn``, and for
``Right(value).fold(leftFn, rightFn)`` we don't use its ``leftFn``. To
avoid problems with linters or TSServer, we can ignore non-used
parameters using the underscore. See the :ref:`v1 impl Left Right`
below.

.. info::

   Several programming languages use the underscore ``_`` U+5f LOW
   LINE to indicate that the parameter in that position should be
   ignored.


Left and Right Unit Tests
-------------------------

Pay special attention how the unit tests assert that ``Left().map()``
**DOES NOT** apply the provided function to the value, and the value
remains unmodified.

Also, notice that we assert that ``fold()`` applies the *left*
function for ``Left(v).fold(l, r)`` and the *right* function for
``Right(v).fold(l, r)``.

.. literalinclude:: /../src/composable-fp-js/vid03/Either.spec.js
   :language: javascript


.. _v1 impl Left Right:

Implementation of Left and Right
--------------------------------

.. literalinclude:: /../src/composable-fp-js/vid03/Either.js
   :language: javascript

================================================================
04 - Use chain for composable error handling with nested Eithers
================================================================

Here's the `video lesson`_.

.. _`video lesson`:
   https://egghead.io/lessons/javascript-use-chain-for-composable-error-handling-with-nested-eithers

This example involves parsing and returning a port number from a
JSON config file.

**REMEMBER**: You can always read the `full source code for these
examples in the Gitlab repository`_.

.. _`full source code for these examples in the Gitlab repository`:
  src-vid04_

.. _`src-vid04`:
   https://gitlab.com/devhowto/Dev-How-To/-/tree/main/src/composable-fp-js/vid04


v1 - An Initial, Traditional Implementation
-------------------------------------------

This first implementation uses very standard, traditional style of
JavaScript programming for the given situation.

.. literalinclude:: /../src/composable-fp-js/vid04/vid04a.js
   :language: javascript

Everything looks fine, right‽

v2 - Capture Try Catch With Either
----------------------------------

At this point, we attempt to capture the try/catch, either/or
condition with our ``Either`` type.

The tests:

.. literalinclude:: /../src/composable-fp-js/lib/tryCatch.spec.js
   :language: javascript

And the ``tryCatch`` implementation:

.. literalinclude:: /../src/composable-fp-js/lib/tryCatch.js
   :language: javascript

And here's the ``getPort`` implementation using the shiny and new
``tryCatch``:

.. literalinclude:: /../src/composable-fp-js/vid04/vid04b.js
   :language: javascript


We are handling errors while attempting to read the file, but we may
still get errors while parsing the JSON in case it is invalid.


v3 - Problems With Invalid, Unparsable JSON
-------------------------------------------

Suppose the JSON is invalid:

.. code-block::

   $ cat vid04/config-04c.json
   {
     invalid: 'json'
   }

This is the current implementation of ``getPort``:

.. code-block:: js

   const getPort = (configPath) => {
     return tryCatch(() => readFileSync(configPath))
       .map(JSON.parse)
       .fold(_ => 3000, objCfg => objCfg.port);
   };

The above would produce the following results:

.. code-block:: text

   log(getPort());
   // → 3000

   log(getPort('./vid04/wrong-path.json'));
   // → 3000

   log(getPort('./vid04/config-04c-invalid.json'));
   // → SyntaxError: Unexpected token i in JSON at position 4

So, we can handle no file incorrect file path, but not invalid JSON.

v4 - tryCatch JSON.parse
------------------------

Well, we can just ``tryCatch`` ``JSON.parse`` just like we are doing
with ``readfilesync``.

.. code-block:: js

   const getPort = (configPath) => {
     return tryCatch(() => readFileSync(configPath))
       .map(jsonCfg => tryCatch(() => JSON.parse(jsonCfg)))
   };

   log(String(getPort('./vid04/config-04c-invalid.json')));
   // → Right(Left(SyntaxError: Unexpected token i in JSON at position 4))

We now have used ``tryCatch`` twice, one to handle ``readfilesync``
exceptions, and another one to handle ``JSON.parse`` exceptions.

We intentionally did not ``fold`` to attempt to return the port to
show a problem with this approach. If you pay attention to the result,
we have a ``Left`` inside a ``Right``. That is, a container inside a
container. This situation makes it hard to reason about the
code... More over, if we just ``fold`` after we parse the JSON, we get
``undefined`` because of the nesting of containers. Handling that
without touching our ``Either`` implementation is possible but becomes
harder and harder as the nesting of containers gets deeper and deeper.

**TODO**: Add an example of how to the manual un-nesting of containers.

Explained in a different way:

.. code-block:: js

   const getPort = (configPath) => {
   // <1>
   return tryCatch(() => readFileSync(configPath))
     .map(jsonCfg => tryCatch(() => JSON.parse(jsonCfg)))
     // <2>
     .fold(_ => 3000, objCfg => objCfg.port);
   };

#. Return a ``Right(fileContent)`` because we can read the file just
   fine.

#. Now we map, and try to parse the json, which is impossible because
   it is not valid json and it throws an exception, which tryCatch
   handles and returns a Left(err). Now we have a Right(Left(err)). A box
   within a box.

**PROBLEM**: We get a box inside a box... 😭

v5 Using .chain()
-----------------

What we can do is to write a ``chain()`` method to be used in place of
``map()`` sometimes.

Chain is just like ``map()`` except we don't “box it back up” so we end
up with **one** ``Left`` or ``Right`` afterwards.

.. code-block:: js
   :emphasize-lines: 3

   function Left(value) {
     return {
       chain: _ => Left(value),
       map: _ => Left(value),
       fold: (leftFn, _) => leftFn(value),
       toString: () => `Left(${value})`,
     };
   }


``Left().map()`` and ``Left().chain()`` always ignore the function and
refuse to apply it (we are handling error conditions).

On the other hand, ``Right().map()`` does apply the function and box
the result, while ``Right().chain()``/ applies the function but does
not box the value.

.. code-block:: js
   :emphasize-lines: 3

   function Right(value) {
     return {
       chain: f => f(value),
       map: f => Right(f(value)),
       fold: (_, rightFn) => rightFn(value),
       toString: () => `Right(${value})`,
     };
   }

.. tip::

   Think like this: “If we are going to return another ``Either``,
   use ``chain()`` instead of ``map()``.” This is the golden tip to
   know when to use ``chain()`` instead of ``map()``.

Check the full definition of ``Either`` in :ref:`helper either
implementation`.

Then, the implementation of ``getPort()`` using chain goes like this:

.. code-block:: js

   const getPort = (configPath) => {
     return tryCatch(() => readFileSync(configPath))
       .chain(jsonCfg => tryCatch(() => JSON.parse(jsonCfg)))
       .fold(_ => 3000, objCfg => objCfg.port);
   };

Then, even with invalid JSON, we still get a default port of 3000.

.. code-block:: js

v6 Final Implementation
-----------------------

We finally reach our best implementation (from the point of view of
FP).


getPort Unit Tests
~~~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid04/getPort.spec.js
   :language: javascript

getPort Implementation
~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid04/vid04d.js
   :language: javascript

We do not check whether it is an error or not because we have a
composable type that will do the right thing when mapped or folded.

Thoughts
--------

We have learn how to use these utilities in context, how to combine
and use them appropriately for each given situation. ``chain()`` is
defined just like our initial ``Box().fold()`` was. Our current
``fold()`` implementation takes a left and a right function, and it
captures the idea of removing a value from its context (from its “Box”
or “Container”). ``chain()`` expects use to run a function and return
another one. This is a key thing to keep in mind:

- ``fold()`` applies a function and unbox the value, which also means
  we cannot keep chaining invocations after that.
- ``chain()`` applies a function, but returns another function, still
  able to be chained.

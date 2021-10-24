================================================================
05 - A collection of Either examples compared to imperative code
================================================================

Here's the `video lesson`_.

.. _`video lesson`:
   https://egghead.io/lessons/javascript-a-collection-of-either-examples-compared-to-imperative-code

Remember that when we use ``fromNullable()`` we are actually returning
an ``Either`` type: either a ``Left`` or a ``Right`` container which
enables us to chain invocations in a composable, functional stylish
approach not requiring assignments and the use of imperative style.


openSite()
----------

An example of a function that would return the login page for non
logged in (unauthenticated) users and a welcome page for logged in
(authenticated) users.

Unit Tests
~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid05/openSite.spec.js
   :language: javascript

Helper Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A few helper functions used in the ``openSite()`` function.

.. literalinclude:: /../src/composable-fp-js/vid05/openSite-helpers.js
   :language: javascript

Imperative Style
~~~~~~~~~~~~~~~~

We are using a *conditional operator* here (instead of the *if else*
as in the video because the condition in this example is short and
concise enough that using the lengthier *if else* doesn't gain us any
readability in this case.

.. tip::

   We colloquially say *ternary operator*, but the name used in the
   spec is indeed *conditional operator*. See the `MDN docs on the
   conditional operator`_ and the `spec on the conditional operator`_.

.. _`MDN docs on the conditional operator`:
   https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Conditional_Operator

.. _`spec on the conditional operator`:
   https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-conditional-operator

.. literalinclude:: /../src/composable-fp-js/vid05/openSite-v1.js
   :language: javascript

Functional, Composable Style
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid05/openSite-v2.js
   :language: javascript


getPrefs()
----------

In this example, the ``getPrefs()`` function returns default
preferences for non-premium (free tier) users, and the user
preferences for premium users.

Unit Tests
~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid05/getPrefs.spec.js
   :language: javascript

TypeDefs
~~~~~~~~

We also have some JSDoc types so we get nice intellisesnse and
documentation while coding! This is no TypeScript. This is Vanilla
JavaScript, but TSServer does an incredible job with the types from
the JSDoc comments!

.. literalinclude:: /../src/composable-fp-js/vid05/getPrefs-typedefs.js
   :language: javascript

.. image:: /../docs/composable-fp-js.assets/vim-getPrefs-jsdoc-intellisense.png

Imperative Style
~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid05/getPrefs-v1.js
   :language: javascript

Functional, Composable Style
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this case, we are not using the ``Either`` type (``Left`` and
``Right``) to indicate error and success conditions, but to capture
non-premium and premium user contexts, and chain logic in a composable
way based on that.

.. literalinclude:: /../src/composable-fp-js/vid05/getPrefs-v2.js
   :language: javascript

You will notice that I did not include the ``loadPrefs()`` as in the
video. A few reasons for this decision are:

- It didn't seem important for the example.

- Calling ``loadPrefs()`` from inside ``getPrefs()`` seems
  wrong. ``getPrefs()`` sounds like a *getter*, or a *reader accessor*
  (in Ruby parlance) that should be used for that single and only
  purpose.


getStreetName()
---------------

This example is about getting a street name from an address. But what
if we don't have an address value/object? And if we have that, what if
street is missing from that address object? Those potential problems
are the topic of this next example.

.. note::

   In the video, perhaps for conciseness, the function is named
   ``streetAddress``. That does not sound like a function, but as a
   variable. Functions and methods should be like verbs “normal”
   variables, classes and other similar things should be like
   nouns. So, ``streetName`` is a variable which presumably contains
   the string name of a street, while ``getStreetName`` is a function
   that would, given some input, retrieve/parse/extract the name of
   the street. In this example, I have decided to follow this
   rationale.

Unit Tests
~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid05/getStreetName.spec.js
   :language: javascript

Imperative Style
~~~~~~~~~~~~~~~~

This is the example precisely as in the video.

.. literalinclude:: /../src/composable-fp-js/vid05/getStreetName-v1.js
   :language: javascript

To be fair, we can *try to* improve its imperative style a little. Let
us of destructuring to avoid things like ``const street =
address.street``, that is, repeat ‘street’ twice, and make use of
early returns to remove some of the nesting inside the ``if`` blocks.

.. literalinclude:: /../src/composable-fp-js/vid05/getStreetName-v2.js
   :language: javascript

The above is still repetitive, though. With a library like Ramda_ we
could do something as simple as this:

.. code-block:: js

   function getStreetName(user) {
     return pathOr('no street', ['address', 'street', 'name', user]);
   }

.. _`ramda pathOr`:
   https://ramdajs.com/docs/#pathOr

.. _Ramda:
   `ramda pathOr`_

As you see, no nesting and no complex logic. We can also use
`ECMAScript Optional Chaining (?.)`_ to help write concise and clean
code:

.. code-block:: js

   function getStreetName(user) {
     return user?.address?.street?.name ?? 'no street';
   }

Note that we also used the `Nullish coalescing operator (??)`_ avoid
values other than ``undefined`` and ``null`` to be treated as falsy
values.

.. _`ECMAScript Optional Chaining (?.)`:
   https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining

.. _`Nullish coalescing operator (??)`:
   https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Nullish_coalescing_operator

Functional, Composable Style
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With this example we finally bring our ``Either`` type into the
game. Recall that ``fromNullable`` returns either a ``Left`` or a
``Right`` container.

.. literalinclude:: /../src/composable-fp-js/vid05/getStreetName-v4.js
   :language: javascript

Note that we ``chain`` on the ``address`` because we don't want a
container inside a container. Then, we ``map`` over ``street`` and
retrieve its ``name`` property. That is feed into ``fold``. If
``name`` is falsy (like ``undefined`` or an empty string), ``fold``
runs the left function which returns ‘no street’; otherwise it applies
the identity function (``id``) to simply return the name of the street.

We could change the syntax a bit to reduce the number of things like:

.. code-block:: text

   address => address.street
   street => street.name

... so it becomes more like this:

.. code-block:: text

   ({ street }) => street
   ({ name }) => name

Here it is:

.. code-block:: js

   function getStreetName({ address }) {
     return fromNullable(address)
       .chain(({ street }) => fromNullable(street))
       .map(({ name }) => name)
       .fold(_ => 'no street', id);
   }

I don't think it is necessarily better, but it is nice as well.

concatUniq()
------------



Unit Tests
~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid05/concatUniq.spec.js
   :language: javascript

.. _vid05-concatUniq-imperative-style:

Imperative Style
~~~~~~~~~~~~~~~~

The original code in the video for the imperative style is this:

.. code-block:: javascript

   function concatUniq(x, ys = []) {
     const found = ys.filter(y => y === x)[0];
     return found ? ys : ys.concat(x);
   }

But many things can be considered *falsy* (zero, empty strings, among
other things), which means, if ``x`` is, for instance, 0, and ``ys``
contains zero, it is “found” but the way the condition is being tested
is falsy (because zero is falsy) and we'll append another zero to an
array which already contains zero. It will not “concat uniquely”. It
will duplicate values in some cases.

.. code-block:: javascript

   concatUniq(0, [1, 0, 2]);
   // → [1, 0, 2, 0]

I therefore changed to explicitly only consider something to be found
if the result is ``undefined``. ``Array.prorotype.filter`` returns an
empty array if no element in the array being filtered satisfies the
predicate. But note the code uses *subscript notation* and attempts to
retrieve the element at index 0, which produces ``undefined`` if the
``filter`` finds nothing and returns an empty array.

.. code-block:: javascript

   function concatUniq(x, ys = []) {
     const found = ys.filter(y => y === x)[0];
     return found !== undefined ? ys : ys.concat(x);
   }


Perhaps this would **convey intent** a little better‽

.. code-block:: javascript

   function concatUniq(x, ys = []) {
     const found = ys.filter(y => y === x).length > 0;
     return found ? ys : ys.concat(x);
   }

Here's the full example with JSDocs:

.. literalinclude:: /../src/composable-fp-js/vid05/concatUniq-v1.js
   :language: javascript


Functional, Composable Style
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: /../src/composable-fp-js/vid05/concatUniq-v2.js
   :language: javascript

Note that here we went back to using subscript notation to retrieve
the first element of the filtered array (returned from ``filter``). It
works fine because ``formNullable`` internally uses ``isNil``, which
correctly only considers ``undefined`` and ``null`` to be falsy (nil)
values. In fact, we cannot use the ``.length > 0`` -- which produces a
boolean -- in this case precisely because of ``isNil``.

We also used ``_`` in ``fold`` left and right functions to ignore the
parameters (and avoid linters and TSServers complaints about unused
variables. We only use variables from the enclosing scope inside the
``fold()``.

Final Notes
-----------

Interesting comment from Brian for a question in this video:

   "I think the best practice is to return Either's from low level
   partial functions that might return null. If they do that, then
   when you're at an application level, you've ensured existence at
   the edges so you don't end up with a bunch of unnecessary
   (paranoia-driven-development) checks in your main logic."

   -- Brian Lonsdorf

===========
02 Currying
===========

Exercise A
----------

.. container:: qanda

   .. container:: question

      .. code-block:: js

         // words :: String -> [String]
         const words = str => split(' ', str);

      Refactor to remove all arguments by partially applying the function.

   .. container:: answer

      .. code-block:: js

         const words = split(' ')


Exercise B
----------

.. container:: qanda

   .. container:: question

      .. code-block:: js

         // filterQs :: [String] -> [String]
         const filterQs = xs => filter(x => x.match(/q/i), xs);

      Refactor to remove all arguments by partially applying the functions.

   .. container:: answer

      As you can see, it is much more concise and sexy.

      .. code-block:: js

         const filterQs = filter(match(/q/i));


Exercise C
----------

.. container:: qanda

   .. container:: question

      Considering the following functions.

      .. code-block:: js

         const keepHighest = (x, y) => (x >= y ? x : y);

         // max :: [Number] -> Number
         const max = xs => reduce((acc, x) => (x >= acc ? x : acc), -Infinity, xs);

      Refactor ``max`` to not reference any arguments using the helper
      function ``keepHighest``.

   .. container:: answer

      const max = reduce(keepHighest, -Infinity);


An existing line!

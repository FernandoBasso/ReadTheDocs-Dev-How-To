==========================
02 - First Class Functions
==========================

functions are first-class
-------------------------

.. container:: qanda

   .. container:: question

      .. code-block:: js

         const hi = name => `Hi, ${name}`;
         const greet = name => hi(name);

         greet('Ahsoka');
         // → "Hi, Ahsoka"

      There is a big no-no in ``greet``'s definition. Something is reduntant. Explain.

   .. container:: answer

      Functions are *first-class* in ECMAScript. If ``greet`` just
      gets an argument and merely forwards it to ``hi``, then there is
      no need to make ``greet`` take that parameter and pass it along
      to ``hi``. We can just as well do this:

      .. code-block:: js

         const greet = hi;

      The result is the same, and we didn't create an unnecessary step
      in the wiring up of functions.

      Read more about the `useless use of functions in JavaScript`_.

controller and views
--------------------

.. container:: qanda

   .. container:: question

      .. code-block:: js

         const BlogController = {
           create(attrs) { return Db.create(attrs); },
         };

      Following the same train of thought of the question above, how
      can we simplify this to avoid receiving “The Useless Use of
      Functions Award”‽

   .. container:: answer

      .. code-block:: js

         const BlogController = {
           create: Db.create,
         };

      If ``Db.create`` takes an attributes parameter, then we can just
      make ``create`` be a reference to ``Db.create`` and the result
      is the same. And no awards.


.. _Useless use of functions in JavaScript:
   https://fernandobasso.dev/javascript/useless-use-of-functions-in-javascript.html

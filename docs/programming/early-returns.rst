========================
Guards and Early Returns
========================

We can combine code guards with early returns to avoid nesting logic
inside ifs, making use of the so called *early returns*. For example,
instead of this:

.. code-block:: js

   function f(x) {
      if (x > 0) {
      // Logic NESTED inside if.
      }
   }

We can do this:

.. code-block:: js

   function f(x) {
       if (x < 0) return 'Oops...';

       // Main logic NOT nested inside if.
   }

By using this second approach, we reduce the need to write logic too deeply nested inside of ifs.

========================
Bash Parameter Expansion
========================

remove newline
--------------

Similar to ``tr -d '\n``, we can also do in pure bash:

.. code-block:: bash

   "${var//[$'\t\r\n']}"



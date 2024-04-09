bash regex support
------------------

The syntax ``if [[ $1 =~ ^[0-9]+$ ]]`` should work since 3.0-alpha, or
just use ``[[ $1 && $1 != *[!0-9]* ]]``.

#!/bin/bash

: <<-'////'
--------------------------------------------------------------------------------
= Arithmetic Operations With Floats
===================================

https://www.hackerrank.com/challenges/bash-tutorials---arithmetic-operations

printf %.3f\\n "$((1000 * 10 / 3))e-3"

--------------------------------------------------------------------------------
////

read -r expression

printf '%.3f\n' "$( echo "scale=4; $expression" | bc -l )"

: <<-'////'
--------------------------------------------------------------------------------
Take a look:

    expression='5 + 50 * 3 / 20 + (19 * 2) / 7'

    $ echo "$expression" | bc -l
    17.92857142857142857142

    $ echo "scale=3; $expression" | bc -l
    17.928

    $ echo "scale=4; $expression" | bc -l
    17.9285

Now, see what printf does if you use %.3f with the results:

    $ printf '%.3f\n' 17.928
    17.928

    $ printf '%.3f\n' 17.9285
    17.929

With 17.9285, printf %.3f rounds 85 to 9! That is the expected answer for this
challenge. People who used scale=3 for bc ended up with the 17.928 result,
instead of 17.929. So, either use no scale in bc, or use scale=4 or more.

    $ printf '%.3f\n' $( echo "$expression" | bc -l )
    17.929

    $ printf '%.3f\n' $( echo "scale=3; $expression" | bc -l )
    17.928

    $ printf '%.3f\n' $( echo "scale=4; $expression" | bc -l )
    17.929

The first and last are correct (as per the challenge), The middle one is not.

And, printf does rounding, looks like rounds up down from 5 (not 4), and up
from 6 (not 5):

    $ printf '%.2f\n' 5.915
    5.91

    $ printf '%.2f\n' 5.916
    5.92


== Other Considerations
-----------------------

There is this “trick” of multiplying the result of the expression by 1000 and
use the e-N thing.

Not the difference caused by the order in which sub expressions are evaluated:

    $ printf '%.6f\n' $(( 1000 * 10 / 3 ))e-3
    3.333000

    $ printf '%.6f\n' $(( 1000 * (10 / 3) ))e-3
    3.000000

Probably best to simply use bc.


== Links and Resources
----------------------

http://mywiki.wooledge.org/BashFAQ/022

http://www.tldp.org/LDP/abs/html/ops.html

https://unix.stackexchange.com/questions/40786/how-to-do-integer-float-calculations-in-bash-or-other-languages-frameworks


--------------------------------------------------------------------------------
////

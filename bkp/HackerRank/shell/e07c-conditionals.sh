#!/bin/bash

: <<-'////'
--------------------------------------------------------------------------------
= Getting Started With Conditionals
===================================

https://www.hackerrank.com/challenges/bash-tutorials---getting-started-with-conditionals/problem

== Running
----------

    $ bash script.sh
    y<Enter>

Or

    $ bash script.sh <<<'Nope'

--------------------------------------------------------------------------------
////

read -r answer

printf '%s\n%s\n' YES NO | grep -i "^${answer}.*"


: <<-'////'
--------------------------------------------------------------------------------
We are printing yes and no in two lines:

    $ printf '%s\n%s' YES NO
    YES
    NO

Then, we grep the output so only matching lines will still make it to
STDOUT. Use a regex and case-insenstivie grep flag, -i.


    $ bash script.sh <<<'nO'
    NO

    $ bash script.sh <<<'YE'
    YES

    $ bash script.sh <<<'yEs'
    YES

--------------------------------------------------------------------------------
////


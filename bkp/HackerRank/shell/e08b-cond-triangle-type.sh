#!/usr/bin/env bash

: <<-'////'
--------------------------------------------------------------------------------
Conditionals Type of Triangle
=============================

https://www.hackerrank.com/challenges/bash-tutorials---more-on-conditionals/problem
--------------------------------------------------------------------------------
////


types=(EQUILATERAL ISOSCELES SCALENE)

count=$( cat | tr ' ' '\n' | sort -n -u | wc --lines )

echo "${types[$((count - 1))]}"

: <<-'////'
--------------------------------------------------------------------------------
https://www.hackerrank.com/challenges/bash-tutorials---more-on-conditionals/problem

Solution based on the number of repeated lengths.

First, get the input, replace whitespace with newlines:

    $ tr ' ' '\n' <<<'3 2 5'
    3
    2
    5

Then, numerically sort the lines

    $ echo $'2\n3\n1' | sort --numeric-sort
    1
    2
    3

And make each value unique.

Here, tree different numbers, three output lines, which means this is
a scalene triangle:

    $ echo $'2\n3\n1' | sort --numeric-sort --unique
    1
    2
    3

Here, 2 is repeated twice, but it appears only once in the output because
of --numeric-sort --unique. Two output lines, which means this is an
isosceles triangle:

    $ echo $'2\n3\n2' | sort --numeric-sort --unique
    2
    3

The sorting and unique causes the 2 to be output only once, thus, only
one output line, which means this is an equilateral triangle:

    $ echo $'2\n2\n2' | sort --numeric-sort --unique
    2

Finally, wc --lines counts the number of output lines, which we use to deduce
the type of the triangle.

With that, we can count how many different times each number appears and
conclude the type of the triangle from that:

Equilateral: The output is one line. Same number tree times was used as input.

Isosceles: The output is two lines. Same number twice, and one other number.

Scalene: The output is three lines. Three different numbers as input.

--------------------------------------------------------------------------------
////


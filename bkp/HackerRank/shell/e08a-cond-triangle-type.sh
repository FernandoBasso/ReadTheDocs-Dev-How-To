#!/usr/bin/env bash

: <<-'////'
--------------------------------------------------------------------------------
Conditionals Type of Triangle
=============================

https://www.hackerrank.com/challenges/bash-tutorials---more-on-conditionals/problem
--------------------------------------------------------------------------------
////


read -r a
read -r b
read -r c

[[ "$a" == "$b" ]] && [[ "$b" == "$c" ]] && echo EQUILATERAL && exit

[[ "$a" != "$b" ]] && [[ "$b" != "$c" ]] && [[ "$c" != "$b" ]] && echo SCALENE && exit

echo ISOSCELES

: <<-'////'
--------------------------------------------------------------------------------
https://www.hackerrank.com/challenges/bash-tutorials---more-on-conditionals/problem

Solution based on side lengths.

  a == b && b == c            = equilateral
  a != b && b != c && c != a  = scalene
  otherwise                   = isosceles

--------------------------------------------------------------------------------
////


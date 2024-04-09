#!/usr/bin/env ruby -wU
# coding: utf-8

strings = 'leonardo donatello raphael mechaelangelo'.split ' '

#
# Remember that ruby methods/functions return the value of its last expression
# by default. A lambda is an anonymous function (a method, actually), but still
# a function, therefore it returns its last expression.
#
# Here, we are storing the lambda in a variable.
#
symbolize = lambda {|arg| arg.to_sym}

#
# If a lambda is referenced from a variable, you must use `&` when
# passing it just like you do when passing a Proc.
#
symbols = strings.collect(&symbolize)
puts symbols.inspect

#
# `collect/select` → ruby
# `filter`  → javascript
#

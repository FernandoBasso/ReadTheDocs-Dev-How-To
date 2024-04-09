#!/usr/bin/env ruby

#
# Four types of scope:
#  - Local
#  - Global
#  - Instance
#  - Class

$global = 'global'
@instance = 'instance variable # belongs to an object'

class Person
    @@classvar = 'belongs to a class'
end

foo = 'local'
_foo = 'also local'

MY_CONST = 'contant' # but can be alterered, with a warning, though.

#
# Pseudo-variables
# nil → uninitialized variables
# self → current object (this).
#

num = 10
puts defined? num
if defined? num == 'local-variable'
    puts "`num` is a local variable."
end


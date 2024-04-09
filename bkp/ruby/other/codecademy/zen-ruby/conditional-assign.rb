#!/usr/bin/env ruby -wU

foo = nil # no value so far.

#
# Using the conditional assignment, we only assign Yes! to
# foo if it doesn't already have a value.
#
foo ||= 'Yes!'

puts foo # Yes!

#
# Try another conditional assignment. Since foo already
# has a value at this point, the assignment won't happen.
#
foo ||= 'Yes indeed.'

puts foo # Still Yes!

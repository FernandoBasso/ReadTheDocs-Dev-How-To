#!/usr/bin/env ruby -wU

#
# Method.
#
def foo
    puts 'method str 1'
    yield # Allows this method to take a block
          # as parameter.
    puts 'method str 2'
end

#
# Block using { and }.
#
foo { puts 'Hey' }

#
# Block using do/end keywords.
#
foo do
    puts 'May the force be with you.'
end

#
# Why do some methods accept a block and others don't? It's because methods that
# accept blocks have a way of transferring control from the calling method to the
# block and back again. We can build this into the methods we define by using the
# yield keyword.
#


#!/usr/bin/env ruby -wU

megagerie = {
    :foxes => 2,
    :giraffe => 1,
    :weezards => 17,
    :elves => 1,
    :canaries => 4,
    :ham => 1
}

#
# Symbols are ‘names’, but don't think of them as strings.
#
# While there can be multiple strings with the same value,
# there can be only copy of a particular symbol at a given
# time.
#

#
# These two are not the same.
#
puts 'string'.object_id
puts 'string'.object_id

#
# But these two are indeed the same.
#
puts :symbol.object_id
puts :symbol.object_id

#
# NOTE: `object_id` is how ruby determines if two ‘things’
# are actually the same thing.
#

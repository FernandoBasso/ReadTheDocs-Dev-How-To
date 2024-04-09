#!/usr/bin/env ruby -wU

#
# An array of friends.
#
masters = [
    'Master Yoda',
    'Obi-Wan Kenobi',
    'Qui-Gon Jinn',
]

#
# A hash of family names and “roles”.
#
family = {
    'Cati' => 'mom',
    'Lucas' => 'son',
    'Mateus' => 'son',
}

# Iterate over array.
masters.each { |master| print "#{master}\n" }

# Iterate over hash. Note --> key, val.
family.each { |key, val| print "#{key} →  #{val}\n" }


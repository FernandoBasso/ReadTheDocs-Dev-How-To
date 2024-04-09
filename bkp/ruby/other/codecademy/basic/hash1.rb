#!/usr/bin/env ruby -wU

#
# Ruby hashes are like associative arrays in PHP, or
# dictionaries in Python.
#

#
# Let's create a hash using the “literal” notation.
#
my_hash = {
    'name' => 'Yoda',
    'age' => 900,
    'skill' => 'The Force',
}

puts my_hash['name']
puts my_hash['age']
puts my_hash['skill']

# Adding items to an existing hash.
my_hash['foo'] = 'bar'

puts my_hash['foo'];

#
# NOTE:
# We use { and } insteado of [ and ] when *defining* the hash.
# We use 'key' => 'value' pairs. Note that there is a ‘=>’ this time.
# We still use commas to separate items.
# We use [ and ] when *accessing* elements.
#


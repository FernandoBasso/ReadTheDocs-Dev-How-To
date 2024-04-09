#!/usr/bin/env ruby -wU

#
# A hash of family names and “roles”.
#
family = {
    'Cati' => 'mom',
    'Lucas' => 'son',
    'Mateus' => 'son',
}

#
# If you do `some_hash.each` and pass define only one argument
# to use as the key inside the block, it will actually not
# be the key, but the whole “row”.
#

family.each do |key|
    print "#{key}\n" # Will NOT print only the keys.
end
# → ["Cati", "mom"]
# → ["Lucas", "son"]
# → ["Mateus", "son"]

#
# Use each_key if you only want the keys.
#
family.each_key { |key| puts key }
# → Cati
# → Lucas
# → Mateus

#
# REMEMBER: `puts` does some work before printing values of arrays, hashes
# and other types of objects so that it works a bit like var_dump in php.
#

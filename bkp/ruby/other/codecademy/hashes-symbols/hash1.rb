#!/usr/bin/env ruby -wU

#
# LITERAL NOTATION
#
my_hash = {
    'yoda' => 'Master Yoda',
    'vader' => 'Dart Vader'
}

#
# CONSTRUCTOR NOTATION
#
my_other_hash = {
    'obi-wan' => 'Obi-Wan Kenobi',
    'luke' => 'Luke SkyWalker'
}

my_other_hash.each do |key, val|
    puts "#{key}: #{val}"
end

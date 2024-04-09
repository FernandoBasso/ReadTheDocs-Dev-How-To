#!/usr/bin/env ruby -wU

# 'Yoda'.length
#
# 'Yoda' is an instance of the String class. length is a method
# that can be called on strings.
#

#
# An example on how to create our own class.
#

class Language
    def initialize(name, creator)
        @name = name
        @creator = creator
    end

    def description
        puts "I'm #{@name} and I was created by #{@creator}."
    end
end

#
# Creates some instances of our Language class.
#
ruby = Language.new('Ruby', 'Yukihiro Matsumoto')
python Language.new('Python', 'Guido van Rossum')
javascript Language.new('JavaScript', 'Brendan Eich')

#
# Call methods we defined in our class.
#
puts ruby.description
puts python.description
puts javascript.description


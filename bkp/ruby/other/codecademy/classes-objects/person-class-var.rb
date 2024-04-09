#!/usr/bin/env ruby -wU

# Sunday, 04:52 - April 12 - 2015

#
# This class shows the use of both instance and class variables.
# @var is for instance variable; @@var is for class variables.
#
class Person

    # Class instance (all objects point to it)
    @@people_count = 0

    def initialize(name)
        @name = name
        @@people_count += 1
    end

    # Can call this method on Person instead of a Person instance.
    def self.number_of_instances
        @@people_count
    end

end

matz = Person.new('Yukihiro')
dhh = Person.new('David')

# We have two instances of Person.
puts "Number of Person instances: #{Person.number_of_instances}"


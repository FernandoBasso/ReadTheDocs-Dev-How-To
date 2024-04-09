#!/usr/bin/env ruby

# Defines a class.
class Person
    def say_hi
        puts 'Hello there'
    end
end

# Creates an object out of it.
obj = Person.new

# Passing false to .methods() cause it to only show mehtods that
# are defined in the object itself, not in the class. So far,
# we have only defined `say_hi` in the class.
puts obj.methods(false) # Nothing shows up.

#
# A class is an instance of the Class class. So, a class is itself
# an object.
#

# .intance_methods(false) shows methods defined in the class itslef.
puts Person.instance_methods(false) # Shows say_hi.

# Add a new method to an instance of Person (not to the class Person)
def obj.say_bye
    puts 'Bye'
end

# And now it shows the method `say_bye`.
puts obj.methods(false)


# (12:18) < FernandoB> I did p = Person.new. How to get only methods I wrote in
#                      the Person class myself?
# (12:18) < FernandoB> p.methods(false) doesn't show the ones I defined.
# (12:22) <     jhass> >> class Person; def foo; end; end;
#                      Person.new.methods(false)
# (12:22) <    ruboto> jhass # => [] (https://eval.in/308533)
# (12:22) <    hanmac> >> class Person; def foo; end; end;
#                      Person.instance_methods(false)
# (12:22) <    ruboto> hanmac # => [:foo] (https://eval.in/308534)
# (12:26) <     jhass> "If the optional parameter is false, it returns an array
#                      of obj's public and protected singleton methods"
# (12:26) <     jhass> emphasis on singleton here
# (12:27) < sevenseac> FernandoBasso: what's Person?
# (12:28) <     jhass> >> class Person; end; p = Person.new; def p.foo; end;
#                      p.methods(false)
# (12:28) <    ruboto> jhass # => [:foo] (https://eval.in/308535)
# (12:46) < FernandoB> So, obj.methods(false) only shows methods added later to
#                      an already-instantiated object?
# (12:49) <   apeiros> FernandoBasso: no
# (12:49) <   apeiros> FernandoBasso: it shows all methods which are only defined
#                      on the object itself
# (12:49) <   apeiros> and not e.g. via its class, or the ancestry of its class
# (12:50) <   apeiros> the point in time does not matter.
# (12:52) < FernandoB> Ah, I see. Thank you folks.
# (13:41) < FernandoB> And why did they name the method `instance_methods` if we 
#                      call it from a class name, not from an object?
# (13:42) <     jhass> it returns the instance methods defined in the class object
# (13:42) <     jhass> the instance methods of a class object will become the 
#                      methods of the classes instances
# (13:43) <    centrx> FernandoBasso, a class is an instance of the Class class

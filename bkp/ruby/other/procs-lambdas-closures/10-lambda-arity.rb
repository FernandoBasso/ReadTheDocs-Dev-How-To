#
# One key difference between lambdas and procs is that lambdas
# enforce arity (they are picky about number of args).
#

#
# If the lambda's block expects three params,
#
hello = lambda do |name, age, skill|
    p "#{name}, #{age}, #{skill}"
end

#
# ...one has to invoke it with three arguments.
#
hello.call 'Yoda', 900, 'The Force'

# Trying different number of args will cause an exception. You can neither
# pass less nor more args than the number established in the lambda's definition.
#

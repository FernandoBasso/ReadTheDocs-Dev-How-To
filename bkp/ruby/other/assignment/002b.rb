
# Variables created in an outer scope are not visible in inner methods scope,
# unlike do/end and {} blocks.

foo = 'try me';

def show_foo
  puts foo
end

#
# Altough `foo` is not visible inside `show_foo`, running this code
# does not produce an error. Why not?
#
# Because whe didn't call `show_foo`. Call it and the error pops it.
#

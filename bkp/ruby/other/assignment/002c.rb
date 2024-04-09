
# Variables created in an outer scope are not visible in inner methods scope,
# unlike do/end and {} blocks.

foo = 'try me';

def show_foo
  puts foo
end

show_foo

# → 002c.rb:8:in `show_foo': undefined local variable or method `foo' for main:Object (NameError)
# →         from 002c.rb:11:in `<main>'

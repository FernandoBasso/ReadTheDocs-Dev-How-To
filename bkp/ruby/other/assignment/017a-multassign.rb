
foo, bar = 10, 20

puts foo + bar
# →  30




# ----------------------------------------------------
def my_method=(val)
  # this works.
  p assigned: val

  # but this does not
  # assigned: val
  # p assigned
end

self.my_method, $my_global = 'yoda', 'luke'
p $my_global
# →  "luke"
# →  {:assigned=>"yoda"}


def do_test=(val)
  my_var = val
  puts "Assigned: #{my_var}"
end

# self needed sinse we did not use () and = far away.
self.do_test, $other_global = 10, 20
puts $other_global



# ----------------------------------------------------
# Multiple assignment can swap two values “in place”.



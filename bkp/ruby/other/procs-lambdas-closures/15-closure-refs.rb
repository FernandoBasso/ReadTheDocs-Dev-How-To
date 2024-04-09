
def run_it p
    p.call
end

name = 'Yoda'

# You might think, “well, a this point `name` is ‘Yoda’, therefore, later
# when I run this proc it will print ‘Yoda’.”. Nope.
#
# Ruby keeps a reference to the variable, not a copy of its value
# at this point.
#
print_a_name = proc { p name }

name = 'Luke'

run_it print_a_name
    # → Luke, because `name` in the proc is a reference, not a hard copy
# of the value of `name` at the point where the proc was created.
#

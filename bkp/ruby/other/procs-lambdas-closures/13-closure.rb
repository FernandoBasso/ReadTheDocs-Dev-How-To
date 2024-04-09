
def run_it p
    # <2> and when we run the proc, `name` is still accessible.
    p.call
end

name = 'Luke Skywalker'

# <1> `name` is available inside the proc.
print_a_name = proc { puts name }

run_it print_a_name


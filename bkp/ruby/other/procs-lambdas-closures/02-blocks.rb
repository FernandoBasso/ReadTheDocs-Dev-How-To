#
# We can make our own method to take blocks as arguments.
#

def run_block
    # yields control to the passed block.
    yield(10) if block_given?
end

run_block

run_block do |num|
    p 'hey there'
    p num
end

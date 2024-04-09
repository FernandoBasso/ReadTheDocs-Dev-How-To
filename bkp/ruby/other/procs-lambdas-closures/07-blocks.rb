#
# Since ruby 1.9, there are four ways to call/run a block.
#

my_proc = Proc.new do |arg|
    p "I am a proc and I was passed “#{arg}”."
end

# Parens not required here.
my_proc.call('foo')

# .() runs `call` behind the scenes, and it works for other stuff
# if they implement `call` as well.
my_proc.('strange')

my_proc[30]

my_proc === 100

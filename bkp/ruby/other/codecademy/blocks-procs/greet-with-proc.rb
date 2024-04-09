#!/usr/bin/env ruby -wU

#
# Procs
# -----
#

#
# In this case, you have a method with a `yield` part, then create a proc and
# store it in a variable, then call the method passing the proc with &.
#
# & converts the proc to a block.
#

def greeter
    yield
end

phrase = Proc.new do
    puts 'Hello there!'
end

greeter(&phrase)

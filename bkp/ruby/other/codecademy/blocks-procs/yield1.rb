#!/usr/bin/env ruby -wU

#
# Passing parameters to `yield`
# -----------------------------
#

#
# Example 1
# ---------


#
# Define the method and set it up for the block arg accordingly.
#
def salutation(name)

    print 'Greetings, '

    #
    # `name` becomes `who` later when the block is
    # passed the method salutation.
    #
    yield(name)
end

#
# Call the method, passes the arg, passes the arg block, and
# uses the method arg inside the block through the variable `who`.
#
salutation('Master Yoda') do |who|
    puts "#{who}. I bow before your might."
end
# → Greetings, Master Yoda. I bow before your might.

#
# The method takes an argument. It can use the argument, and it can also pass
# that argument to `yield`. The block then must be able to deal with arguments
# (|arg| or |arg1, arg2| stuff, you know).
#


####################################################################################################
# Example 2
# ---------

def yield_name(name)
    puts 'Begin'
    yield('Yoda')
    puts 'Middle'
    yield(name)
    puts 'End'
end

#
# Using { and } to delimit the block.
#
yield_name('Luke') { |n|
    puts "The name is #{n}."
}
# → Begin
# → The name is Yoda.
# → Middle
# → The name is Luke.
# → End

#
# Using do/end to delimit the block.
#
yield_name('Obi-Wan Kenobi') do |n|
    puts "The name is #{n}."
end
# → Begin
# → The name is Yoda.
# → Middle
# → The name is Obi-Wan Kenobi.
# → End



################################################################################
#
# The same as above, but with a proc this time.
#

#
# The method takes the name as argument as normal.
#
def salutation(name)
    yield(name)
end

#
# Define the proc, nonchalantly... as if nothing was going to
# happen (but we know it will).
#
greet = Proc.new do |someone|
    puts "Greetings, #{someone}."
end

#
# Call the method passing the name -and- the proc.
#
salutation('Obi-Wan', &greet)


#
# REMEMBER that when the method takes no paramemter, we can still pass
# a proc to it as parameter? Well, in this case, altough we defined
# only the name as parameter, we can still pass a extra one: the proc.
#



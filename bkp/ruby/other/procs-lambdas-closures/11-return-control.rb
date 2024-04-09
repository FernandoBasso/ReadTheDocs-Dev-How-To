def run_it(a_proc)
    p 'Ini'
    a_proc.call
    p 'End'
end

run_it lambda { p 'The Lambda'; return }

puts '---------------------------------------'

# Will never get to the 'End' line. Tries to return
# from here.
run_it proc { p 'The Proc'; return }

#
# A return in a proc, will try to do the return from the context
# where the proc is defined (not where it is currently running).
#

#
# A lambda returns to the context where it was invoked.

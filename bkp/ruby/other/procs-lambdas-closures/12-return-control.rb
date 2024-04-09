#
# A return in a proc, will try to do the return from the context
# where the proc is defined (not where it is currently running).
#

#
# A lambda returns to the context where it was invoked.
#


def run_sth(prc)
    p 'Starting...'
    prc.call
    p 'Ending...'
end

def program
    #
    # Invert these two lines and run the program to
    # see what happens.
    #

    run_sth lambda { p 'A Lambda'; return }
    run_sth proc { p 'A Proc'; return }
end

program

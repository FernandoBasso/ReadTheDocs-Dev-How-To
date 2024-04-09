#!/usr/bin/env ruby -wU

#
# Very importante differences are pointed out below.
#

def batman_ironman_proc
    victor = Proc.new { return 'Batman will win!' }
    victor.call

    # Procs don't pass control back to the # calling method. This string will
    # never # be returned.
    'Iron Man will win.'
end

def batman_ironman_lambda
    victor = lambda { return 'Batman will win!' }
    victor.call

    # Still runs because lambdas pass control back
    # to the calling method.
    'Iron Man will win.'
end

puts batman_ironman_proc
puts batman_ironman_lambda


#
# Lambdas and procs have only two differences.
#
#
# - Lambdas enforce arity. A lambda checks the number of arguments. A
#   proc does not, ignoring extra ones and adding nil to the missing
#   ones. Lambdas raise errors when arguments are incorrect.
#
# - When a lambda returns, it passes the control back to the
#   calling method. When I proc returns, it does so immediately
#   without going back to the calling method.
#


#!/usr/bin/env ruby -wU

#
# Like procs, lambdas are objects. The similarities don't stop there: with the
# exception of a bit of syntax and a few behavioral quirks, lambdas are
# identical to procs.
#

lambda { puts 'Hello!' }

#
# Other example.
#
def lambda_demo(a_lambda)
    puts "I'm the method!"
    # Call the lambda that is passed as parameter.
    a_lambda.call
end

#
# Call lambda_demo and create a lambda on the fly.
#
lambda_demo(lambda { puts "I'm the lambda." })

#
# As you see, it is very similar to JavaScript anonymous
# functions (a different name for lambda). In js we would
# do something like:
#
#   lambda_demo(function () { console.log( "I'm the lambda." ); });
#


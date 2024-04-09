
#
# If name isn't suplied, use default 'World'.
#
def hello(name = 'World')
    puts "Hello, #{name.capitalize}!"
end

hello           # call without () and without params.
hello()         # call with () but no params.
hello('luke')   # call passing an argument.
hello 'yoda'    # Also works withouth () and still passing args.

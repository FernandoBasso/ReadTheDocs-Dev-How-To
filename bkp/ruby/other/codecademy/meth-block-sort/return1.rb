#!/usr/bin/env ruby -wU

def add(a, b)

    #
    # Need those parenthesis or we get syntax errors
    # due to precedence.
    #
    #return false unless (a.is_a? Integer) && (b.is_a? Integer)
    # or this
    return false unless a.is_a? Integer and b.is_a? Integer

    return a + b
end

puts add(2, 'n')
puts add(2, 3)

# (20:22) < FernandoB> Can I do something like this?
# (20:22) < FernandoB> return false unless (a.is_a Integer && b.is_a? Integer) ?
# (20:22) < zenspider> did you try?
# (20:23) < FernandoB> Yes, and got unexpected tCONSTANT, expectingsyntax error,
#                      unexpected tCONSTANT, expecting ')'
# (20:23) < FernandoB> Oh, dear.
# (20:23) < FernandoB> My paste was messed up. Sorry.
# (20:23) < zenspider> we got it
# (20:24) <   apeiros> FernandoBasso: you'll want to add parens
# (20:24) < zenspider> needs more parens: ruby -cwe 'return false unless
#                      (a.is_a?(Integer) && b.is_a?(Integer))'
# (20:24) < zenspider>
# (20:24) <   apeiros> FernandoBasso: or use `and`, but personally, I prefer
#                      parens
# (20:25) < FernandoB> So, it was a precedence problem?
# (20:25) < zenspider> personally I like: `return false unless a.is_a? Integer
#                      and b.is_a? Integer`
# (20:26) < zenspider> but I wouldn't write that. I'd raise ArgumentError on each
#                      one individually
# (20:26) < zenspider> which takes care of the precedence problem :)
# (20:26) <   apeiros> FernandoBasso: yes. precedence problem.

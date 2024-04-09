
#
# Variables created in an outer scope are visible in inner -block- scopes, but
# not inside method scope.
#

sneaky = 'hey there';

1.times do
  foo = 9
  bar = -11
  puts %Q(Local variables: #{local_variables.join ', '})
end
# →  Local variables: foo, bar, sneaky

puts "Outer scope variables: #{local_variables.join ', '}"
# →  Outer scope variables: sneaky


# FernandoBasso | “You may isolate variables in a block from the outer scope by listing them
#               | following a ; in the block’s arguments. See the documentation for block local
#               | variables in the calling methods documentation for an example.”
# FernandoBasso | Where is that found?
#         Radar | FernandoBasso: Citation needed.
# FernandoBasso | I am reading this, by the way:
#               | http://ruby-doc.org/core-2.1.2/doc/syntax/assignment_rdoc.html
#         Radar | FernandoBasso: Look in the sidebar. calling_methods.rdoc
#         Radar | FernandoBasso:
#               | http://ruby-doc.org/core-2.1.2/doc/syntax/calling_methods_rdoc.html
# FernandoBasso | Ah, "in the calling methods documentation"...
# FernandoBasso | Radar: silly me.
# FernandoBasso | Thanks.
#         Radar | FernandoBasso: np
#        Ox0dea | FernandoBasso: I'm not sure why that's in the 2.1 documentation; the semicolon
#               | hasn't been necessary since 1.9.
#        Ox0dea | 18>> foo = 1; proc { |foo| foo = 2 }.call(0); foo
#        ruboto | Ox0dea # => 2 (https://eval.in/430253)
#        Ox0dea | 19>> foo = 1; proc { |foo| foo = 2 }.call(0); foo
#        ruboto | Ox0dea # => 1 (https://eval.in/430254)
#        Ox0dea | In short, feel free to pretend you never knew you could stick semicolons in
#               | block argument lists. :P

# FernandoBasso | I misunderstood the docs.
# FernandoBasso | I was thinking it would be possible to make the a variable (in the example)
#               | not to "leak" into the block.
# FernandoBasso | What the call_method doc talks about is a way not to let outer variables
#               | interfere with block variables with the same name, it seems.

#       drbrain | Ox0dea: your example shows a different thing than what block local arguments
#               | does
#       drbrain | >> foo = 1; proc { foo = 2 }.call; p foo
#        ruboto | drbrain # => 2 ...check link for more (https://eval.in/430255)
#       drbrain | >> foo = 1; proc { |;foo| foo = 2 }.call; p foo
#        ruboto | drbrain # => 1 ...check link for more (https://eval.in/430256)
#        Ox0dea | Oh, right.
#       drbrain | block local arguments allow you to make arbitrary variables behave like block
#               | arguments
#       drbrain | FernandoBasso: ↑
# FernandoBasso | ✓
#        Ox0dea | It puts me in mind of K&R-era function definition.




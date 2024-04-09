#
# BEWARE of the precedence.
#

arr = 1, 3, 5

# `puts' gets `arr.map' as argument, and the block is bound to `arr.map'.
puts arr.map { |e| e * 2} # <1>
# →  2
# →  6
# →  10

# `puts' receives both `arr.map' and the block as arguments.
puts arr.map do |e| e * 2 end # <2>
# → #<Enumerator:0x000055a9f3394138>


#
# <1> is the same as:
#
#   puts (arr.map { |e| e * 2 })
#
# <2> is the same as:
#
#   puts (arr.map) do |e| e * 2 end # <2>
#
# In the second case, the code block is interpreted as being part of the call
# to puts, not the call to map. And if you call puts with a block, it ignores
# the block. So the do/end version is really equivalent to
#
#   puts array.map
#
# If you supply a code block but the method you call doesn’t see it (or doesn’t
# look for it), no error occurs: methods aren’t obliged to yield, and many
# methods (including map ) have well-defined behaviors for cases where there’s
# a code block and cases where there isn’t. If a method seems to be ignoring a
# block that you expect it to yield to, look closely at the precedence rules
# and make sure the block really is available to the method.
#

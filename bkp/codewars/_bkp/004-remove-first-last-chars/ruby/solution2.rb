
##
# Removes the first and last chars of a string.
#
# ASSUME: string is at least three chars long.
#
# @param [string] s
# @return [string]
#
chop_sides = ->(s) { s[1...-1] }

p chop_sides.call('eloquent')
# ⇒ loquen

p chop_sides.call('country')
# ⇒ ountr

p chop_sides.call('person')
# ⇒ erso

p chop_sides.call('place')
# ⇒ lac


#
# Ruby docs and Rubocop require parenthesis when a stabby lambda takes
# parameters. Also, they use no space between `->` and `()`
#
#   ->(arg) { }  # good
#
#   -> (arg) { } # bad
#
# But this is OK (no arguments, no parenthesis, space is OK, even required):
#
#   -> { }
#
# https://docs.ruby-lang.org/en/2.6.0/Proc.html
#
# https://www.rubydoc.info/gems/rubocop/RuboCop/Cop/Style/StabbyLambdaParentheses
#


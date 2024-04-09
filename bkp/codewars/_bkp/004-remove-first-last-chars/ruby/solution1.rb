
##
# Removes the first and last chars of a string.
#
# ASSUME: string is at least three chars long.
#
# @param [string] s
# @return [string]
#
def chop_sides(s)
  s[1...-1]
end

p chop_sides('eloquent')
# ⇒ loquen

p chop_sides('country')
# ⇒ ountr

p chop_sides('person')
# ⇒ erso

p chop_sides('place')
# ⇒ lac


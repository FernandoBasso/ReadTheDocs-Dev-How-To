
##
# @param [s] string
# @return string
get_middle = lambda do |s|
  #
  # The base case.
  #
  return s if s.size <= 2

  #
  # Recurse until we reach the base case.
  #
  get_middle.call(s[1..-2])
end

p get_middle.call('OS')
# ⇒ OS

p get_middle.call('Linux')
# ⇒ n

p get_middle.call('TypeScript')
# ⇒ Sc

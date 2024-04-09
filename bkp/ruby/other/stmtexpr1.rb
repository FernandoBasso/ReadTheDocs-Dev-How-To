
#
# Ruby statements are actually expressions, just like
# in “Lisp”. Isn't that something?
#

x, y = 10, 15

num1 = if x > y then 'yes' else 'no' end
num2 = x < y ? 'yes' : 'no'

puts num1
puts num2
# →  no
# →  yes

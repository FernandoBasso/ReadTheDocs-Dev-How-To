
# `p foo` is similar to `puts foo.inspect`.

str = 'hi'
num = '11'

puts str
p str
# →  hi
# →  "hi"

puts num
p num
# →  11
# →  '11'

# In short, p is much more suitable for debugging.

# IMPORTANT:
# Also, `puts` honnors to_s in a class, whereas `p` does not.

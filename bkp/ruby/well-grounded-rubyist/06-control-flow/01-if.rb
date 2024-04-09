x, y = 5, 9

if x < y
  p x
end

# Using `then'.
if y > x then p y end

# Or using semicolons to mimic line breaks.
if y > x ; p y ; end

if x > y
  p 'x > y'
elsif x == y
  p 'x == y'
else
  p 'x < y'
end

# Same as `not (x < y)'
if not x < y
  p 'boo'
end

# Leaving the parenthses would be like `(!x) < y'.
if !(x < y)
  p 'boo'
end


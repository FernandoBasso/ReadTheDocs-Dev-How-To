
# Blocks create scope.

1.times do
  foo = 1
  bar = 2
  puts local_variables.empty?     # →  false
  puts local_variables.length     # →  2
  puts local_variables.join ', '
end
# →  false
# →  2
# →  foo, bar

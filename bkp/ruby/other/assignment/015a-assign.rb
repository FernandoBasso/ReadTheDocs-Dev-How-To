
# If foo is nil or false, assign a value to it.
foo ||= 'jedi'
p foo
# →  'jedi'


# If foo is NOT nil or false, assign 'yoda' to it.
foo &&= 'yoda'
p foo
# →  'yoda'

# Since foo is not nil or false by now, does nothing.
foo ||= 'new value'
p foo
# →  'yoda'. Not that the value did not change.

#
# `nil` means “no value at all”.
# `false` means a false value.
#
# false is a value, nil is a ‘no value’ value.
# We can even say that nil is a special value
# that means ‘no value’. We could also say
# that nil is not a value...
#

#
# In ruby, only false and nil are ‘falsy’ values. Everything
# else, including 0, '', empty arrays and hashes are ‘truthy’
# values.
#

creatures = {
    'puppies' => 0,
    'kittens' => 3
}

puts creatures['kittens']
puts creatures['cows'] # nil, but no error.

myhash = Hash.new('this is a default value')
puts myhash['foo'] # 'this is a default value'

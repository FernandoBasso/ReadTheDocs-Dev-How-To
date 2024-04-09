
#
# Only `true` and `false` are booleans. All other values (incuding `nil` can
# be used as booleans, but they are not real booleans.
#

#
# Only `false` and `nil` are falsy.
#
# 0, '', [] and {} are all truthy values when used in a boolean context.
#


'' || 'yes'
# → ''

'' && 'yes'
# → yes

'' && 0 && {} && [] && 'all truthy'
# → all truthy

puts 'truthy' if {}
# → 'truthy'


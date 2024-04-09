
arr = [10, 'foo', :cloth, 20, 'bar', :price]

#
# `collect` returns whatever you tell it to return for each item on the
# original array depending on whether it passes the test. The resulting array
# has the same length of the original array. If you test for truthiness or
# falsehood, it returns that. If you concatenate the item with '-foo', it
# returns that, etc.
#
# `select` retuns only the elements that pass the test. The resulting
# array contains only those elements.
#

#
# COLLECT 1
#
res1 = arr.collect do |item|
    item.is_a? Symbol
end
puts res1.inspect
# → [false, false, true, false, false, true]

#
# COLLECT 2
#
res2 = arr.collect do |item|
    item.to_s + '-foo'
end
puts res2.inspect
# → ["10-foo", "foo-foo", "cloth-foo", "20-foo", "bar-foo", "price-foo"]

#
# SELECT
#
res3 = arr.select do |item|
    item.is_a? Symbol
end
puts res3.inspect
# → [:cloth, :price]

#
# » collect/map     → ruby
# » map             → javascript
#
# » select          → ruby
# » filter          → javascript
#
# http://stackoverflow.com/questions/5254732/difference-between-map-and-collect-in-ruby
#


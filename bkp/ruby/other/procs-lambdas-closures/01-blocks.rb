
#
# Blocks are essentially “nameless functions” or “nameless portions
# of code.”
#

arr = [10, 20, 30, 40]

arr.each do |el|
    p el * 2
end
# → 20
# → 40
# → 60
# → 80

arr.each { |el| p el * el }
# → 100
# → 400
# → 900
# → 1600

#
# `each` is an iterator method that accepts a code block
# as argument. For each element of the array, the block
# is run on that element.
#


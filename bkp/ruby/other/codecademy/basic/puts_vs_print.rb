
arr2d = [['a1', 'a2'], ['b1', 'b2'], ['c1', 'c2']]

puts arr2d
# → a1
# → a2
# → b1
# → b2
# → c1
# → c2


print arr2d
# → [["a1", "a2"], ["b1", "b2"], ["c1", "c2"]]

print "\n----------------------------------------\n"

#
# `print` helps you more clearly in some cases when you want to see
# what the contents of an array are.
#
my_hash = {
    'jedi' => 'yoda',
    'skill' => 'the force',
    'age' => 900
}

#
# We are sorting the hash by its key, and that returns an -array-.
#
arr = my_hash.sort_by { |k, v| k}

#
# Question: What is the structure of the returned array?
#

#
# `puts arr` gives you no clue what the format of the arr contents is.
# print is much more ‘clueful’ in this case.
#
print arr
# → [["age", 900], ["jedi", "yoda"], ["skill", "the force"]]


# Wheares puts would only show you each item on a separate line.

#
# STILL, you would do `puts arr.inspect` :)
#

 #
 #
 #

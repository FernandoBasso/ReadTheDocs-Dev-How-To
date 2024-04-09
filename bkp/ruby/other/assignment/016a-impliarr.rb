
# Here, since we are passing multiple values, ruby assumes we
# want to create an array.
arr = 'foo', 'bar', 'jedi'
p arr.class
# →  Array


# FernandoBasso | What is the difference between arr = 1, 2, 3 (which I got), arr = *[1, 2, 3]
#               | and arr = 1, *[2, 3]. The last two seem to result in just the same thing as
#               | the first one.
#         shevy | well the first variant is the lazy variant of arr = [1, 2, 3]
# FernandoBasso | yes.
# FernandoBasso | Ah, a, b, c = *[1, 2, 3] ?
#         shevy | the * is the splat operator:
#               | https://endofline.wordpress.com/2011/01/21/the-strange-ruby-splat/
#         shevy | a, b, c = *[1, 2, 3] would be the same as  a, b, c = [1, 2, 3]
# FernandoBasso | I see.
# FernandoBasso | Well, the doc is (sorry), -useless- about splat then (at least the part I am
#               | reading now).


#
# Read about “splat”
#
# in http://ruby-doc.org/core-2.2.0/doc/syntax/calling_methods_rdoc.html
#

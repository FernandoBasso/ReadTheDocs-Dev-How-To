##
# Returns a lambda that computes the area of a rectangle.
#
# The math formula is:
#
#   A = base * length
#
area ->(b, l) { b * l }

#
# The area of a triangle is computed by the following formula:
#
#   A = 1/2 * base * length
#

area_rectangle = area(2, 3).call
area_triangle = (1 / 2) * area(2.0, 3.0).call

p area_rectangle
p area_triangle

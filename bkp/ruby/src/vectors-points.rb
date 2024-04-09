#
# Moving a character to a specific location by creating a
# vector between two points.
#

class Vector
  attr_accessor :x, :y

  def to_s
    "x: #{x}, y: #{y}"
  end
end

class Point
  attr_accessor :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  # Implement a _subtract_ method to subtract points.
  # Subtract points to create a new vector.
  def -(p)
    v = Vector.new
    v.x = @x - p.x
    v.y = @y - p.y
    v
  end
end

point_player = Point.new(0, -1)
point_enemy = Point.new(1, 1)

# Vector v is Point p - Point i. Point has an instance method `-', but Ruby
# adds some syntax sugar on top of it to make it possible for us to call
# it in such a way that looks like an operator.
# Same as `point_player.-(point_enemy)'.
enemy_vector = point_player - point_enemy

# Enemy has to move to these coordinates to get closer to the player.
puts enemy_vector
# → x: -1, y: -2


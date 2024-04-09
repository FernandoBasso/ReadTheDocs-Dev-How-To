class Cake
  def initialize(batter)
    @batter = batter
    @baked = true
  end
end

class Egg
end

class Baker
  def bake_cake
    @batter = []
    pour_flour
    add_egg
    stir_batter
    Cake.new(@batter)
  end

  def pour_flour
    @batter.push(Flour.new)
  end

  def add_egg
    @batter.push(Egg.new)
  end

  def stir_batter
  end

  private :pour_flour, :add_egg, :stir_batter
end

baker = Baker.new

#
# What determines whether you can call a private method isn’t the object you’re
# sending the message to, but which object is self at the time you send the
# message.
#
# Ruby creates _private_ methods by forbidding an explicit receiver. Instance
# methods have `self' as the receiver. We can't just call `add_egg' from the
# toplevel because the toplevel doesn't have that method.
#


class Coder
  def initialize(name)
    @name = name
    @level = 0;
  end

  def level
    @level
  end

  def master?
    @level >= 100
  end

  def practice
    @level = @level + 10
    p "Got to level #{@level}"
  end
end

##
# Practice until you become a master.
#
coder = Coder.new('Aayla Secura')

loop do
  break if coder.master?

  coder.practice
end
#
# → "Got to level 10"
# → "Got to level 20"
# → "Got to level 30"
# → "Got to level 40"
# → "Got to level 50"
# → "Got to level 60"
# → "Got to level 70"
# → "Got to level 80"
# → "Got to level 90"
# → "Got to level 100"
##

developer = Coder.new('Ahsoka Tano')

developer.practice until developer.master?
#
# → "Got to level 10"
# → "Got to level 20"
# → "Got to level 30"
# → "Got to level 40"
# → "Got to level 50"
# → "Got to level 60"
# → "Got to level 70"
# → "Got to level 80"
# → "Got to level 90"
# → "Got to level 100
##

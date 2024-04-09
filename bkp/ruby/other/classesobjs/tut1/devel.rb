class Jedi

  def initialize
    @skill = 'The Force'
  end

  def get_skill
    return @skill
  end

end

jedi = Jedi.new
puts jedi.get_skill
# →  Nothing shows up but an empty line.



class Jedi

  attr_writer :skill
  attr_reader :skill

  attr_accessor :birthday

  def meth=(name)
    @name = name
  end

  def name
    @name
  end

end

jedi = Jedi.new

# Using the “setters”.
jedi.meth = 'Yoda'
jedi.skill = 'The Force'
jedi.birthday = 25

# Using the “getters’.
p jedi.name
p jedi.skill
p jedi.birthday

# output
# →  "Yoda"
# →  "The Force"
# →  25


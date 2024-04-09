#
# You can't define an instance variable and try to access it directly. You always
# need getters/setters because instance variables are private.
#

class Jedi

  #
  # Here, we try to set a default skill, but it doesn't work as intended.
  # Setting it here has no effect. What we really need is to use
  # `initialize` and set `@skill` there.
  #
  @skill = 'The Force'

  #
  # Getter for `@skill`.
  #
  def get_skill
    return @skill
  end

end

jedi = Jedi.new
puts jedi.get_skill
# →  Nothing shows up but an empty line.

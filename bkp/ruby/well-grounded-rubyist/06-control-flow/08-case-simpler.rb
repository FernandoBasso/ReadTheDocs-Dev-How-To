#
# We can do:
#
#   case <some_obj>
#   when foo_obj
#     # do something
#     exit
#   when bar_obj
#     # do some other thing
#     exit
#   else
#     # mo match...
#   end
#
# But `case' has simpler version in which we _do not_ pass <some_obj> right
# after `case'.
#

class Jedi
  attr_reader :skill
  def initialize(skill)
    @skill = skill
  end
end

j1 = Jedi.new('Levitate')
j2 = Jedi.new('Lightsaber')
j3 = Jedi.new('Grip Through The Screen')

skill = 'Lightsaber'

case
when j1.skill == skill
  puts 'Thanks, but this is not the skill we are looking for.'
  exit
when j2.skill == skill
  puts 'I bow before your might.'
  exit
when j3.skill == skill
  puts 'Not this one either.'
  exit
else
  puts 'What the poop‽'
end



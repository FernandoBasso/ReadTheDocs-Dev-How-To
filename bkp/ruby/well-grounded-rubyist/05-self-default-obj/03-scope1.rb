#
# Instance variables are self-bound rather than scope bound.
#
# Scope and self aren't the same thing. You can change one without necessarily
# changing the other (but sometimes they do change together).
#

$gvar_1 = 'I am global'

class MyExample
  p $gvar_1
  $gvar_2 = 'Created inside the class, but still global'

  def show_gvars
    $gvar_3 = 'Gvar The Third'
    p $gvar_1
    p $gvar_2
  end
end

p $gvar_1
p $gvar_2

# Error, because we did not actually execute show_gvars, which means $gvar_3
# did not actually get created. It is a bit strange because the p lines are
# executed inside the method while ruby is parsing the file, though. Still, p
# is a method invocation, while show_method is a method definition. So, it
# makes a little bit of sense, one could argue.
p $gvar_3

# →  "I am global"
# →  "I am global"
# →  "Created inside the class, but still global"

#
# No mater where you define a gvar, it is available everywhere.
#


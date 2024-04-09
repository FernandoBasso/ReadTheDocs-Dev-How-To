
# GLOBAL VARIABLE identifiers start with the `$` symbol.
# If you don't start with `$`, it will be some other type
# of variable, but not global.

# Declares a global variable.
$myvar = 'the force'

# Declares a method/function. `$myvar` is visible here.
def show_it
  puts $myvar
end

class Something

  # Method inside a class, and the global variable
  # is still visible!
  def say_it
    puts $myvar
  end

end

show_it
# →  the force

Something.new.say_it
# →  the force


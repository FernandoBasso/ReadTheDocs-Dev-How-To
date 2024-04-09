module Stars
  module Masters
    class Jedi
      A_CONST = 'a constant'
      a_cvar = 'a cvar'
    end
    # We can use “relative paths” in a situation like this.
    p Jedi::A_CONST
    # Or the full path, just that it doesn't make much sense...
    p Stars::Masters::Jedi::A_CONST
  end
end

# CONSTANT LOOKUP is like finding a file on the filesystem. You specify a path.
# Can access the constant, even without a reader/getter method. Constants
# have this “global like” behavior. If you follow the path, you can read them.
p Stars::Masters::Jedi::A_CONST
# →  "a constant"

# But this won't work. CVARS and IVARS need proper reader accessors if one
# is to be able to reach them.
p Stars::Masters::Jedi::a_cvar
# →  undefined method `a_cvar' for Stars::Masters::Jedi:Class (NoMethodError)


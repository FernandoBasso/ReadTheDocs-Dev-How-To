
# Global variables start with a dollar sign.
$foo = 0

class Thing

  # Yes, you can just call statements in bare class bodies.
  # The statement below runs just for being here. You don't
  # need to instantiate the class for it to be run. It just runs.
  p "inside the class: #{$foo}"


  def show_it
    p %Q{in a method: #{$foo}}

    # Increment the global `$foo`.
    $foo -= -1

    # Creates yet another global.
    $other = 'another'
  end

end

obj = Thing.new
obj.show_it
obj.show_it
p $other
# →  "inside the class: 0"
# →  "in a method: 0"
# →  "in a method: 1"
# →  "another"


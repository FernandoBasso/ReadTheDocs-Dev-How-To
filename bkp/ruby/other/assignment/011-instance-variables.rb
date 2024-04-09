
class Thing

  def initialize(val)
    # Instance variable.
    @myvar = val
  end

  # A method to access the intance variable.
  def get_it
    @myvar
  end

  def get_non_existent
    # Not initialized. Gets the value `nil`.
    @nope
  end

end


obj = Thing.new 'ruby'
puts obj.get_it
puts obj.get_non_existent


# Run with -w to see warnings.
#
#   ruby -w program.rb

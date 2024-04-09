class Person

  #
  # @count is a CLASS INSTANCE VARIABLE. Only Person can get to it.
  #
  @count = 0

  def initialize
    self.class.count += 1   # <0> →  correct.
    #self.count += 1        # <1> →  error.
    #@count += 1            # <2> →  error.
  end

  def self.count
    @count
  end

  def self.count= value
    @count = value
  end

end

#
# Both <1> and <2> produce errors. <0> Seems to be the only way to go.
#

class Worker < Person

  #
  # @count is a CLASS INSTANCE VARIABLE. Only Worker can get to it.
  #
  @count = 0

end


4.times do
  Person.new
end

2.times do
  Worker.new
end

puts Person.count
# →  4

puts Worker.count
# →  2



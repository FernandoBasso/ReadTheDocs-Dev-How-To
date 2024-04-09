
class Jedi

  @count = 0

  def initialize
    self.class.count += 1
  end

  def self.count
    @count
  end

  def self.count= count
    @count = count
  end

end

Jedi.count = 9
puts Jedi.count

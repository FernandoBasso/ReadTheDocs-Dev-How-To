
class Foo

  attr_accessor :var

  class << self
    attr_accessor :var
  end

  # civar
  @var = 42

  def initialize
    # ivar
    @var = 3.14
  end

  def self.show_it
    p @var
  end

end

Foo.show_it
p Foo.var
foo = Foo.new
p foo.var


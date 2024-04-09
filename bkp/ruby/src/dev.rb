require 'pry'

class MyObj

    def initialize(name, buyable)
        @name = name
        @buyable = buyable
    end

end

obj = MyObj.new 'Tablet', true
p obj

binding.pry

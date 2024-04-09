

#
#      Mon_Ouie | FernandoBasso: @foo = some_value sets the instance variable called @foo of the
#               | object currently referenced to by self to some_value
#         shevy | it reminds me of the lightning talk about javascript
#         shevy |   https://www.destroyallsoftware.com/talks/wat
#
#
#      Mon_Ouie | From one of your previous examples I think you may simply be using the
#               | instance variables of two different objects
#      Mon_Ouie | i.e. when you write class Person; @name = "John"; end, you are creating an
#               | instance variable for the class itself (which is just a special kind of Ruby
#               | object). Instances of that classes that will be created later will not have an
#               | instance variable called @name as a result of this.
#
#
# FernandoBasso | Mon_Ouie: This just prints an empty line: http://sprunge.us/UOSA?ruby
# FernandoBasso | Ah!
#      Mon_Ouie | >> class Foo; self; end
#        ruboto | Mon_Ouie # => Foo (https://eval.in/428601)
#          jher | shevy: wow that bare-words thing shows some crazy metaprogramming
#      Mon_Ouie | The code that is executed when a new instance of a class is created is the
#               | initialize method
#         shevy | jher haha I loved the pictures
#          jher | Reminds me of an XKCD a while back about JS
#


class Person

  @count = 3

  def self.count
    @count
  end

  def self.count= val
    @count = val
  end

  def name
    @name
  end

  def name= name
    @name = name
  end

end

p = Person.new

p.name = 'Yoda'
puts p.name
# →  Yoda

p.name = 'Luke'
puts p.name
# →  Luke

puts Person.count

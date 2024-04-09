#
# Note that when you do obj.prop = value, you are calling a setter.
#

class Jedi

  def name=(name)
    # Here we are calling name= recursively.
    self.name = name
  end

  def name
    @name
  end

  #
  # But in a method with a different name than the instance variable,
  # it is possible to use self.instance_var.
  #
  def my_test_method_1=(val)
    self.name = self.name + ' ' + val
  end

  #
  # The method above would run just fine. It could also be.
  #
  def my_test_method_2=(val)
    @name = @name + ' ' + val
  end

  #
  # Or combination of self.name and @name (in any order).
  #
  def my_test_method_3=(val)

end

jedi = Jedi.new
jedi.name = 'Yoda'

# →  $ ruby -w devel.rb
# →  devel.rb:5:in `name=': stack level too deep (SystemStackError)
# →  from devel.rb:5:in `name='
# →  from devel.rb:5:in `name='
# →  from devel.rb:5:in `name='
# →  from devel.rb:5:in `name='
# →  from devel.rb:5:in `name='
# →  from devel.rb:5:in `name='
# →  from devel.rb:5:in `name='
# →  from devel.rb:5:in `name='
# →  ... 8723 levels...
# →  from devel.rb:5:in `name='
# →  from devel.rb:5:in `name='
# →  from devel.rb:5:in `name='
# →  from devel.rb:15:in `<main>'



#         shevy | FernandoBasso there is no need for self.name= there is it? I mean you call
#               | this recursively so this should not work right?
#         shevy | you need to read in your mind "self.name = name" as "self.name= name" :D
# FernandoBasso | In my earlier example, although def name=(name); self.name = name; end did not
#               | work, a method with another name accepted self.name.
# FernandoBasso | shevy: Me too.
# FernandoBasso | Anki, also.
#         shevy | yeah because you were recursively calling the method
# FernandoBasso | Makes sense.



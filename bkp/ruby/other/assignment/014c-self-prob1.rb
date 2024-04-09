class Jedi

  def name=(name)
    #
    # Can't use self here. Only @ will do (it seems).
    #
    @name = name
  end

  def name
    @name
  end

  def dothis=(val)
    self.name = @name + ' ' + val
    #
    # Could use all @name or all self.name here, or a combination.
    #
  end

end

jedi = Jedi.new
jedi.name = 'Yoda'
jedi.dothis = 'hey'
p jedi.name

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

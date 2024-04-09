#
# http://stackoverflow.com/questions/29112861/what-is-the-use-of-a-variable-in-the-class-body
#
# http://www.railstips.org/blog/archives/2006/11/18/class-and-instance-variables-in-ruby/
#



class Something

  # @foo is only accessible in mehtods of this class. @foo belongs to the
  # class object, not the class itself, not to an instance of this class.
  @foo = 'Class Object Variable'

  # @@bar is a variable that belongs to the class. All instances of this class
  # can access it and they share the same value.
  @@bar = 'Class Variable'

  def create
    @foo = 'jedi'
    puts @foo
  end

end


obj = Something.new
obj.create

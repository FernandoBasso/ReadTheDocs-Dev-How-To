# tags: [self, method, object]

##
# A sample object.
#
class Object
  private

  def hello
    'Hello!'
  end
end

p Object.send(:hello)


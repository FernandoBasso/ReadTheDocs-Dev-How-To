# tags: [self, method, object]

##
# A sample object with a `hello` private method.
#
class Object
  private

  def hello
    'Hello!'
  end
end

p Object.send(:hello)

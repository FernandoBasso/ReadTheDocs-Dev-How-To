# tags: [self, method, object]

def f
  ##
  # Returns the receiver class name (`Object`).
  #
  self.class.name
end

##
# Does `object` have a method, public or private, called `f`?
#
p Object.respond_to?(:f, true)
#
# → true
##

##
# Prints `f`'s receiver call name as it is what `f`'s
# implementaion returns.
#
p f
#
# → "Object"
##

#
# A method that you define at the top level is stored as a private instance
# method of the Object class.
#
def talk
  puts 'Hello'
end

#
# Toplevel methods can and /must/ be called in bareword style because they
# are private, callable only on self, and without an explicit receiver,
# with the exception of private writer methods which musts be called
# with `self' as the receiver.
#
# A toplevel private method is available anywhere because `Object' is
# an ancestor of all other objects, and therefore, they are in the same
# hierarchy.
#

talk
# → Hello

Object.new.talk
# Fails because it tries to call the private (toplevel) method `talk'
# with an explicit receiver.


#
# `puts' and and `print' are examples of built-in toplevel methods of
# `Kernel' (not `Object', like the ones we write on the toplevel).
#
#    $ ruby -e 'puts Kernel.private_instance_methods'
#

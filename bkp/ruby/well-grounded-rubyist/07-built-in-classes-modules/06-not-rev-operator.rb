#
# Open the class string and add shorter reverse in form of an unary operator.
#
class String
  def !@
    self.reverse
  end
end

puts !'may the force'
# → ecrof eht yam

puts (not 'hello')
# → olleh



#
# When a method that takes no arguments share the same name with a variable,
# you've got to use parentheses or self.method to call the method.
#

public

  def sum
    2 + 3
  end

  sum = sum()
  puts sum

  # Using self requires sum method to be public.
  sum = self.sum
  puts sum



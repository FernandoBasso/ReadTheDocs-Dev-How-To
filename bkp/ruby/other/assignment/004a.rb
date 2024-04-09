
foo = 'the force'

def show_it
  puts foo
end

#
# Q: When if you run the code above, even though `foo` is local to “that” scope
# (and therefore not accessible inside the method `show_it`) you don't get
# errors?
#
# A: Because the method `show_it` insn't actually called. It is simply defined.
#
# BEWARE: Incorrect ruby code only produces errors when executed, not when
# it simply exists in the file.
#

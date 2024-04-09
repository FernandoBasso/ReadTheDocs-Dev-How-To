#
# Methods have an implicit begin and. Works if we want `rescue' to take care
# of the entire method.
#

def open_a_file

  #
  # 1. We have an implicit `begin/end' context at the beginning of methods.
  #

  print 'file to open: '
  filename = gets.chomp

  fh = File.open(filename)
  p fh
  fh.each_line { |line| puts line }

  #
  # 2. Causes an exception because we cannot yield a file handler object.
  #
  yield fh

  fh.close

  #
  # 3. And we can use the implicit `begin/end' and simply `rescue' here.  This
  # will catch any exceptions that may occur, in any line inside the method. We
  # may want to catch exceptions about opening a file, but it would also catch
  # exceptions if you have a problem with `gets', for instance.
  #
  rescue
    # If the exception was caused by `gets', this message would not be
    # appropriate.
    puts "Cannot open file ‘#{filename}’."
end

open_a_file

#
# The method above may not be “well designed”, but it does illustrate the
# possibility of the implicit `begin/end' block.
#
#

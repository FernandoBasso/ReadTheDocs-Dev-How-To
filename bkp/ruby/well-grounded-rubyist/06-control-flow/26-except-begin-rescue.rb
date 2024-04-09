def open_a_file
  print 'file to open: '
  filename = gets.chomp

  begin
    fh = File.open(filename)
    p fh
    fh.each_line { |line| puts line }
  rescue
    puts "Cannot open file ‘#{filename}’."

    #
    # We have to explicitly return otherwise the remaining of the method
    # will be executed.
    return
  end

  #
  # 2. Causes an exception because we cannot yield a file handler object.
  # Yet, we are not rescuing from this exception.
  #
  yield fh

  fh.close

end

open_a_file


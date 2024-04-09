#
# ASSUME: there is a `logfile' method.
#

def a_method
  filename = gets.chomp
  begin
    fh = File.open(filename, 'r')
    fh.each_line { |line| puts line }
  rescue => err
    #logfile.puts("User tried to open #{filename}, #{Time.now}")
    #logfile.puts("Exception: #{e.message}")
    p err

    #
    # We do not pass arguments, but `raise' understands that we are
    # re-raising an exception, and uses the same arguments that were
    # used in the parent exception.
    #
    raise
  end
end


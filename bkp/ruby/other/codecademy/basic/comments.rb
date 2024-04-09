#!/usr/bin/env ruby

# This is a comment.

#
# The first line is a especial case, and it is not a comment. #! is called
# shebang, and it is used by the operating system to know which interpreter
# to use for this file, in this case, ruby.
#
# Having that first line in the file allows you to run /path/to/program.rb
# or simply program.rb if it is in your PATH. Just make sure the file is
# executable. Otherwise, run the program with `ruby program.rb`.
#

puts 'foo bar' # comment after a statement.

=begin
This is a long comment
It uses the “Comment Block Markers”.
=end

=begin
puts 'This will not be printed out.'
=end

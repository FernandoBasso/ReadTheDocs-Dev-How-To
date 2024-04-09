class InvalidLineError < Exception
end

def find_line(pattern)
  fh = File.open('file.txt')
  line = fh.gets
  raise InvalidLineError unless line =~ /#{pattern}/i
  return line
end

p find_line('line 1')

begin
  p find_line('foo bar')
rescue InvalidLineError => err
  puts "Not a valid line... I'm sorry Wilson!"
  puts err
end

puts '-' * 80

#
# And don’t forget that exceptions are classes, classes are constants, and
# constants can be namespaced, courtesy of nesting:
#

module TextHandler
  class InvalidLineError < StandardError
  end
end

def take_line(pattern)
  fh = File.open('file.txt')
  line = fh.gets
  raise TextHandler::InvalidLineError unless line =~ /#{pattern}/i
  return line
end

p take_line('line 1')

begin
  p take_line('foo bar')
rescue TextHandler::InvalidLineError => err
  puts "Not a valid line... I'm sorry Wilson!"
  puts err
end


#!/usr/bin/env ruby

class MegaGreeter

    attr_accessor :names

    def initialize(names = 'World')
        @names = names
    end

    # Say hi to everyone.
    def say_hi
        if @names.nil?
            puts 'say_hi, names is empty.'
        elsif @names.respond_to?('each')
            # Then @names is some sort of list. Iterate!!!
            @names.each do |name|
                puts "Hello, #{name}!"
            end
        # Otherwise it must be a single name.
        else
            puts "Hello, #{@names}!"
        end
    end

    # Say bye to everybody.
    def say_bye
        if @names.nil?
            puts 'say_bye, names is empty.'
        elsif @names.respond_to?('join')
            # Then @names is some sort of list. Join the elements.
            puts "Goodbye #{@names.join(', ')}. Come back soon!"
        else
            puts "Goodbye #{@names}! Come back soon!"
        end
    end

end

puts __FILE__
puts $0

#
# If we are running this script/program/file directly (and not from
# another script.
# http://stackoverflow.com/questions/4687680/what-does-if-file-0-mean-in-ruby
# https://www.ruby-lang.org/en/documentation/quickstart/4/
#

#
# Allows us to use MegaGreeter.rb from other code (as a library) without
# worrying that the code below will execute unintentionally. Therefore,
# we can run MegaGreeter.rb directly (to test it, etc), and still use it
# as a library without having to comment/uncomment the lines below.
#

if __FILE__ == $0 then
    mg = MegaGreeter.new
    mg.say_hi
    mg.say_bye

    mg.names = 'John Williams'
    mg.say_hi
    mg.say_bye

    mg.names = ['Yoda', 'Vader', 'Luke', 'Obiwan']
    mg.say_hi
    mg.say_bye

    mg.names = nil
    mg.say_hi
    mg.say_bye
end

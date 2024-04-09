#!/usr/bin/env ruby

# string, please (s → th)
print 'Thtring, pleathe!: '
user_input = gets.chomp
user_input.downcase!

if user_input.include? 's'
    # Replaces ‘s’ with ‘th’.
    user_input.gsub!(/s/, 'th')
else
    puts 'Nothing to do here.'
end

puts "Your string is “#{user_input}”."


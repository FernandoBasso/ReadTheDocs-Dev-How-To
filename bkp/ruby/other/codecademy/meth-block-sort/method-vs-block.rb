#!/usr/bin/env ruby -wU

def capitalize(string)
    puts "#{string[0].upcase}#{string[1..-1]}"
end

capitalize('yoda')
capitalize('leia')

['luke', 'vader'].each {|str| puts "#{str[0].upcase}#{str[1..-1]}"}

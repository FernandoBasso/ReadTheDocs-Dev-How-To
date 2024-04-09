#!/usr/bin/env ruby -wU

puts 'Your native language, please:'
lang = gets.chomp.downcase

case lang
    when 'english' then puts 'Hello!'
    when 'french' then puts 'Bonjour!'
    when 'german' then puts 'Guten Tag!'
    when 'finnish' then puts 'Haloo!'
    else puts 'I don\'t know that language!'
end


=begin
puts 'What\'s your favorite language?'
language = gets.chomp

if language == 'Ruby'
  puts 'Ruby is great for web apps!'
elsif language == 'Python'
  puts 'Python is great for science.'
elsif language == 'JavaScript'
  puts 'JavaScript makes websites awesome.'
elsif language == 'HTML'
  puts 'HTML is what websites are made of!'
elsif language == 'CSS'
  puts 'CSS makes websites pretty.'
else
  puts 'I don't know that language!'
end
=end

#
# Refactor the mess above and use case/when.
#

puts "What's your favorite programming language?"
lang = gets.chomp.downcase

case lang
when 'ruby'         then puts 'Ruby is great for web apps!'
when 'python'       then puts 'Python is great for science.'
when 'javascript'   then puts 'JavaScript makes websites awesome.'
when 'html'         then puts 'HTML is what websites are made of.'
when 'css'          then puts 'CSS makes websites pretty.'
else 'I don\'t understand that language.'
end


#!/usr/bin/env ruby -wKuU

puts 'Text to search through:'
text = gets.chomp

puts 'Word to redact:'
redact = gets.chomp

words = text.split(' ')

words.each do |word|
    if word != redact then
        print word + ' '
    else
        #print 'redacted'.upcase + ' '
        print 'REDACTED '
    end
end


#
# redact v.
#
# to remove words or information from a text before it is printed or made
# available to the public:
#
#   Officers' names are routinely redacted from any publicly released reports.
#
#   Some parts of secret files are available to the public, but heavily redacted.
#


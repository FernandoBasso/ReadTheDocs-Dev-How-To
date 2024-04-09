#!/usr/bin/env ruby -wU

# No problem concatenating and using << with strings.
lang = 'Ruby'
puts 'I love ' + lang
puts 'I love ' << lang

# You must use to_s with numbers, however.
age = 900
puts 'Age is ' + age.to_s # or you get an error.
puts 'Age is ' << age.to_s # idem.

# Using string INTERPOLATION causes conversion happen automatically.
puts "I love #{lang}"
puts "Age is #{age}"

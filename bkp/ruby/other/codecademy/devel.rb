#! /usr/bin/env ruby

def salutation(name)
    yield(name)
end

greet = Proc.new do |someone|
    puts "Greetings, #{someone}."
end

salutation('Obi-Wan', &greet)

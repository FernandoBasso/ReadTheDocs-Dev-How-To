#!/usr/bin/env ruby

def what_up(greeting, *bros)
    bros.each do |bro|
        puts "#{greeting}, “#{bro}”."
    end
end

what_up('May the force be with you', 'Yoda', 'Luke', 'Obi-Wan');

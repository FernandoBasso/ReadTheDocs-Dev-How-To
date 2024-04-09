#!/usr/bin/env ruby -wU

def alphabetize(arr, rev = false)
    if rev
        # Descendent
        arr.sort { |item1, item2| item2 <=> item1 }
    else
        # Ascendent
        arr.sort { |item1, item2| item1 <=> item2 }
    end
end

books = [
    'Heart of Darkness',
    'Code Complete',
    'The Lorax',
    'The Prophet',
    'Absalom, Absalom!'
]

puts "A-Z: #{alphabetize(books)}"
puts "Z-A: #{alphabetize(books, true)}"

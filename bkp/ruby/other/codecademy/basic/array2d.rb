#!/usr/bin/env ruby -wKuU

arr2d = [
    ['foo', 'bar'],
    [10.1, 11.2],
]

arr2d.each do |current_row|
    current_row.each do |current_element|
        puts current_element
    end
end

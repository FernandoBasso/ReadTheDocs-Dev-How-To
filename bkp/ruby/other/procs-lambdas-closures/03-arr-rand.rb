#
# Monkey patch the Array class.
#

class Array
    def random_each
        shuffle.each do |el|
            yield el
        end
    end
end

[10, 20, 30, 40, 50].random_each do |item|
    p item
end



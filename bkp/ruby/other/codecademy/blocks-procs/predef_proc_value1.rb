
nums = [10, 50, 68, 101, 129.3, 1000]

#
# Returns a proc which filters using `lim`.
#
def make_filter(lim)
    Proc.new do |item|
        item < lim
    end
end

#
# Assign the proc to `my_filter`.
#
below100 = make_filter(100)
below1000 = make_filter(1000)

res1 = nums.select(&below100)
res2 = nums.select(&below1000)
puts res1.inspect
puts res2.inspect


#
# NOTE: This is very, very, very similar to partials in JavaScript.
#


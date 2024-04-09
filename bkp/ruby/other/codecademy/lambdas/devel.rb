
nums = [10, 50, 68, 101, 129.3, 1000]

def make_filter(limit)
    Proc.new do |num|
        num < limit
    end
end

less_than_100 = make_filter(100)

sel = nums.select(&less_than_100)
puts sel.inspect

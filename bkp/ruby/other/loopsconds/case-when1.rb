

case
when expr1
  # do this
when expr2
  # do this
else
  # if neither of the above, do this by default
end

hour = 15

case
when hour < 12
    puts "Good Morning"
when hour > 12 && hour < 17
    puts "Good Afternoon"
else
    puts "Good Evening"
end

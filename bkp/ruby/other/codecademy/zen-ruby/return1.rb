#!/usr/bin/env ruby -wU

def add(a, b)
  return a + b
end

# Implicit return, without the keyword return.
def mult(a, b)
  a * b
end

puts add(2, 3) # 5

puts mult(2, 3) # 6

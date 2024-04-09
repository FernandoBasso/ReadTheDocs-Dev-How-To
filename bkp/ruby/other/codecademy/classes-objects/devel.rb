#!/usr/bin/env ruby -wU

class MyClass
    # Define inside a class, still making it global.
    $bar = 'Lorem ipsum.'
end

puts $bar # 'Lorem ipsum.'

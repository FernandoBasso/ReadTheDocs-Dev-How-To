#!/usr/bin/env ruby -wU

# << is a shortcut for arr.push(foo)
# It also works on strings.

arr = [10, 20, 30]
arr << 40
puts arr # [10, 20, 30, 40]

jedi = 'Obi-Wan'
jedi << ' Kenobi'
puts jedi # Obi-Wan Kenobi

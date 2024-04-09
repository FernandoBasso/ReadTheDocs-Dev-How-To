#
# Create an array with a hash's keys sorted by the length of the keys, as
# strings.
#

hash = {
  skill: 'The Force',
  foo: 'bar',
  jedi: 'Yoda',
  1 => 'one',
}


p hash.keys.map(&:to_s).sort { |a, b| a.length <=> b.length }
p hash.keys.map(&:to_s).sort_by { |e| e.length }

p hash.keys.map(&:to_s).sort_by { |e| e.length * -1 }
p hash.keys.map(&:to_s).sort_by { |e| -e.length }
p hash.keys.map(&:to_s).sort { |a, b| b.length <=> a.length }

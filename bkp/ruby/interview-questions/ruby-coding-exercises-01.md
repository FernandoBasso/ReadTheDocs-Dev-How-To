# Ruby Coding Exercises 01


## hash keys to array sorted by length

Create an array with a hash's keys sorted by the length of the keys, as strings.

https://stackoverflow.com/a/33326503/2855955

Input hash:

```ruby
hash = {
  skill: 'The Force',
  foo: 'bar',
  jedi: 'Yoda',
  1 => 'one',
}
```

Sort by length, ascending.

```ruby
p hash.keys.map(&:to_s).sort { |a, b| a.length <=> b.length }

p hash.keys.map(&:to_s).sort_by { |e| e.length }
```

All of the above produce this result:
```
["1", "foo", "jedi", "skill"]
```


```ruby
p hash.keys.map(&:to_s).sort_by { |e| e.length * -1 }

p hash.keys.map(&:to_s).sort_by { |e| -e.length }

p hash.keys.map(&:to_s).sort { |a, b| b.length <=> a.length }
```

All of the above produce the following result:
```
["skill", "jedi", "foo", "1"]
```

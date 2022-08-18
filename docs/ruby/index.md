---
title: Ruby
description: Notes, tips and examples on the Ruby programming languages
---

# Ruby

```{toctree}
---
hidden: true
maxdepth: 6
caption: Ruby
---

unit-tests.md
scope.md
hackerrank-ruby.md
bundler.md
rvm.md
pry-repl.md
```

Ruby is a delightful programming language designed for programmer happiness 💖.

Yukihiro Matsumoto once presented [How Emacs Changed My Life (PDF)](./Yukihiro-Matsumoto-How-Emacs-Changed-My-Life.pdf) which goes into how [Emacs](https://www.gnu.org/software/emacs/) and [emacs-lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/) influenced Matz and the design of the Ruby programming language.
Unfortunately, it seems there is not video recording of it.
[Here's the Slideshare link](https://www.slideshare.net/yukihiro_matz/how-emacs-changed-my-life) for the presentation.

- [I Love Ruby](https://i-love-ruby.gitlab.io/book.html) free, online book (rendered in Asciidoctor) by Karthikeyan A K.
- [Official Ruby Docs](https://ruby-doc.org/).


Nice tools:

- [Ruby Style Guide](https://rubystyle.guide/).
- [Awesome Print](https://github.com/awesome-print/awesome_print).
- [pry (irb alternative with powerful introspection capabilities)](https://github.com/pry/pry).
- [Rubocop Docs](https://docs.rubocop.org/rubocop/).
- [Solargraph Ruby Langauge Server](https://solargraph.org/guides).

## Ruby Concepts and Ideas

- The name of the language is spelled *Ruby* (not RUBY or ruby) because it is a proper noun (therefore, the first letter is capitalized). `ruby` is the name of the program we use to run Ruby programs, like `ruby -w fib.rb`.
- A *mix-in* (also spelled *mix in*) is the including of a module inside a class. We say things like “The Enumerable mix-in module provides...”. See [Ruby doc on modules](https://ruby-doc.com/core/doc/syntax/modules_and_classes_rdoc.html).

## Bits and Bytes of Syntax Suggar

### Hash Values
We can get all the values of a hash by doing this:

```
>> { one: 1, two: 2 }.map(&:last)
=> [1, 2]
```

Which is similar to doing `hash.values`

### Map and Hash to Proc to Get Values

```
>> h = { one: 1, two: 2 }

>> [:one, :two].map(&h)
=> [1, 2]
```

With an array of keys, we map over those keys, and *turn the hash into a proc*😲!

Saw this first in a solution for [resistor color duo exercism challenge](https://exercism.org/tracks/ruby/exercises/resistor-color-duo).

If we define `to_proc` we can use this *trick* for any object.

### Hash Transform Values and &:next to_proc

```
>> {one: 1, two: 2}.transform_values(&:next)
=> {:one=>2, :two=>3}
```


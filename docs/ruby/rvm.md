---
title: RVM Ruby Version Manager | Ruby
description: Some notes and useful tips on using rvm to manage Rubies
---

# RVM | Ruby

## `rvm list known' not showing latest rubies

Published: Sat, July 30, 2022.

```shell-session
$ rvm --version
rvm 1.29.12 (latest) by Michal Papis, Piotr Kuczynski, Wayne E. Seguin [https://rvm.io]

$ rvm list known
# MRI Rubies
(some output redacted)
[ruby-]1.9.3[-p551]
[ruby-]2.7[.2]
[ruby-]3[.0.0]
ruby-head
```

For some reason, `rvm list known` did not show ruby 3.1.2`.
Still, running `rvm install ruby-3.1.2` worked.
`rvm` itself had just been upgraded to the latest version with `nvm get stable`.

- [My comment on StackOverflow (question about Ruby 3.1 on macOS)](https://stackoverflow.com/questions/70672711/whats-the-right-string-to-use-when-installing-ruby-3-1-through-rvm-on-mac-os-bi#comment129235266_70673017).


## Default Ruby on New Shells

Make ruby-3.1.2 the default for new shell sessions:

```shell-session
$ rvm --default use ruby-3.1.2
```

- [Default Ruby on RVM docs](https://rvm.io/rubies/default).

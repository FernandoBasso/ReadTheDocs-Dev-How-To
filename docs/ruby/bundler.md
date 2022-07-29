---
title: Bundler | Ruby
description: Some tips and examples on using Ruby Bundler to installing and managing Ruby gems.
---

# Bundler | Ruby

- [Bundler official site](https://bundler.io/).

## Error Could not find gem 'pry-doc

Consider this `Gemfile`:

```gemfile
source 'https://rubygems.org'

gem 'pry'
gem 'pry-doc'
```

In the past (IIRC), we could run both `bundler` or `bundle` commands interchangeably:

```shell-session
$ bundler install
$ bundle install
```

This is what I see on Jul 2022 with ruby 3.0.0 (installed through `rvm`) on my Arch Linux system:

```
$ ruby --version
ruby 3.0.0p0 (2020-12-25 revision 95aff21468) [x86_64-linux]

$ bundler --version
Bundler version 2.2.3

$ bundle --version
Bundler version 2.2.3

$ which bundler bundle
/home/devel/.rvm/rubies/ruby-3.0.0/bin/bundler
/home/devel/.rvm/rubies/ruby-3.0.0/bin/bundle

$ file "$(which bundle)" ; file "$(which bundler)"
/home/devel/.rvm/rubies/ruby-3.0.0/bin/bundle: Ruby script, ASCII text executable
/home/devel/.rvm/rubies/ruby-3.0.0/bin/bundler: Ruby script, ASCII text executable

$ du -h "$(which bundle)" ; du -h "$(which bundler)"
4.0K	/home/devel/.rvm/rubies/ruby-3.0.0/bin/bundle
4.0K	/home/devel/.rvm/rubies/ruby-3.0.0/bin/bundler
```

So, yeah, we are damn sure both `bundler` and `bundle` are the same program.
And yet this error showed up when using `bundler install`:

```shell-session
$ bundler install
Could not find gem 'pry-doc x86_64-linux' in any of the gem
sources listed in your Gemfile.
```

Tried again with `bundle` (instead of `bundler`) and it worked 😲.
It is worth nothing that as of this writing, the docs only mention `bundle install` and not `bundler install`.

It is also worth nothing that later I could not reproduce this problem and `bundler install` worked too.

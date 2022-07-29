---
title: pry | Ruby
description: Some notes, tips and examples on using pry Ruby REPL
---

# Pry | Ruby

## show-doc

Pry's `show-doc` is not available by default. It is a gem that has be be installed separately, either with `gem install pry-doc` or through `Gemfile` and Bundler`.

```shell-session
mkdir: created directory '/home/devel/pry'

$ vim ~/.pryrc
```

Add `require 'pry-doc'`, save and quit.

Start `pry`.
Either:

```shell-session
$ pry --simple-prompt
```

Or, if using bundler:

```shell-session
$ bundle exec pry --simple-prompt
```

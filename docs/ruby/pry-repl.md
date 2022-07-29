---
title: pry | Ruby
description: Some notes, tips and examples on using pry Ruby REPL
---

# Pry | Ruby

## show-doc

:::{admonition} show-doc is deprecated
:class: warn
As of pry 0.14.1, `show-doc` is deprecated and `show-source -d` is to be used instead.
:::

Pry's `show-doc` is not available by default. It is a gem that has be be installed separately, either with `gem install pry-doc` or through `Gemfile` and Bundler`.

To use it, start a pry session and load `pry-doc`:

```text
$ pry --simple-prompt
>> require 'pry-doc'
=> true

>> show-doc String#each_byte
```

Add `require 'pry-doc'` to `~/.pryrc`.

Start `pry`.
Either:

```shell-session
$ pry --simple-prompt
```

Or, if using bundler:

```text
$ bundle exec pry --simple-prompt
>> show-doc String#each_codepoint
```

:::{admonition} pry-doc gem and show-doc command
:class: attention

Note that we install the gem `pry-doc`, but use `show-doc` in the pry session!
:::

---
title: Conquer of Completion Language Server | vim, nvim
description: Some notes, tips and example configurations for setting up coc.nvim for some languages.
---

# CoC LSP

Go through [CoC docs](https://github.com/neoclide/coc.nvim) to install it and do the basic, general configuration.

## Ruby

Install `coc-diagnostics` or have something like this in `.vimrc` or `init.lua`:

```
let g:coc_global_extensions = [
      \ 'coc-diagnostic',
      \ 'coc-json',
      \ 'coc-yaml',
      \ 'coc-tsserver',
      \ 'coc-eslint',
      \ 'coc-solargraph',
      \ 'coc-css',
      \ ]
```

For nvim and lua init file, add the lines above inside `vim.cmd [[ ... ]]`:

```
vim.cmd [[
  normal vim config stuff here
]]
```

Then, in your Ruby project, suppose you have this `Gemfile`:

```
source "https://rubygems.org"

gem 'solargraph'
gem 'rubocop'
gem 'rspec'
```

Run `bundle install`, run vim/nvim and open a `.rb` file (inside from the project), and run command in nvim:

```
:CocLocalConfig
```

Then add some content like this inside the JSON file:

```json
{
  "codeLens.enable": true,
  "solargraph.useBundler": true,
  "diagnostic-languageserver.linters": {
    "rubocop": {
      "command": "bundle",
      "sourceName": "rubocop",
      "debounce": 101,
      "args": [
        "exec",
        "rubocop",
        "--format",
        "json",
        "--force-exclusion",
        "%filepath"
      ],
      "parseJson": {
        "errorsRoot": "files[0].offenses",
        "line": "location.line",
        "column": "location.column",
        "message": "[${cop_name}] ${message}",
        "security": "severity"
      },
      "securities": {
        "fatal": "error",
        "warning": "warning"
      }
    }
  },
  "diagnostic-languageserver.filetypes": {
    "ruby": "rubocop"
  }
}
```

You should start seeing diagnostics (like syntax errors) and intellisense as you work on the ruby files.

### References

- [coc.nvim repo](https://github.com/neoclide/coc.nvim).
- [coc-diagnostic repo](https://github.com/iamcco/coc-diagnostic).
- [coc-diagnostic rubocop linter config issue](https://github.com/iamcco/coc-diagnostic/issues/64).

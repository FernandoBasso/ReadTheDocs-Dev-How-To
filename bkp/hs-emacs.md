## Haskell & Emacs

* [https://wiki.haskell.org/Emacs](https://wiki.haskell.org/Emacs)
* [https://gitlab.haskell.org/ghc/ghc/-/wikis/emacs](https://gitlab.haskell.org/ghc/ghc/-/wikis/emacs)
* [http://haskell.github.io/haskell-mode/manual/latest/](http://haskell.github.io/haskell-mode/manual/latest/)

Notes relevant to using Emacs to code Haskell, including default emacs functionality, plugins, and haskell-mode.

## haskell-mode basics

My relevant emacs config for haskell-mode:

```lisp
(use-package haskell-mode
  :ensure t)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq
 haskell-process-type 'stack-ghci
 haskell-interactive-popup-errors nil
 haskell-process-path-ghci "stack"
 ;; https://github.com/haskell/haskell-mode/issues/1695
 haskell-interactive-types-for-show-ambiguous nil)
```

In a buffer, do `C-c C-l` (haskell-process-load-file) to load the file in the emacs ghci repl.

## errors

Emacs `simple.el` provides `(next-error)` and `(prev-error)`.

* `M-g n` → `next-error`
* `M-g p` → 'previous-error'

We can do `describe-function` (`M-h f`) and `describe-key` (`M-h k`) to figure if a function is bound to a key, and to what function a key is bound.

## Emacs and LSP

* http://abailly.github.io/posts/a-modern-haskell-env.html

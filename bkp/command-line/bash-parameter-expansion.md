# Bash Parameter Expansion

## Intro for # and %

No mater if we use the `#` or `%` expansion, they always come after the identifier. That is, something like `${name#...}`or `${name%...}`. What changes orientation (comes first or after) is the things to match.

## Match from the beginning with # and

```shell
$ v=src/config/README.md

$ echo $v
src/config/README.md
```

Remove from the beginning of the string up to and including the first `/`:

```
$ echo "${v#*/}"
config/README.md
```

Remove from the beginning of the string up to the last `/` found.

```
$ echo "${v##*/}"
README.md
```

We say that `#` is *non-greedy*; it matches as little as it can. On the other hand `##` **is** *greedy*; it matches as much as it can.

## Match from the end with % and %%

`%/*`removes the shortest string matching `/*` from the end.

```shell
$ v='/etc/conf.d/tmux.conf'

$ echo "${v%/*}"
/etc/conf.d
```

It starts from the end and removes everything until it finds the first “/” (first left to right).

Using `%%` instead of `%` makes it greedy, removing as much as it can:

```shell
$ v='src/config/README.md'

$ echo "${v%%/*}"
src
```

It is **greedy**. It starts at the end and goes until it finds the last “/” in a left-to-right direction.

>  it removes the shortest string matching /* from the end, so it removes /test.conf
> 
> -- @geirha at #bash IRC chanell

## The Pattern

This list is copied from [here](https://mywiki.wooledge.org/BashGuide/Parameters#Parameter_Expansion).

* `${parameter#pattern}`: The 'pattern' is matched against the **beginning** of 'parameter'. The result is the expanded value of 'parameter' with the shortest match deleted.

* `${parameter##pattern}`: As above, but the *longest* match is deleted.

* `${parameter%pattern}`: The 'pattern' is matched against the **end** of 'parameter'. The result is the expanded value of 'parameter' with the shortest match deleted.

* `${parameter%%pattern}`: As above, but the longest match is deleted. 



So, if we want to match from the from the beginning up to the first "/", we do `#*/`. `*` means *any* char and `/` is the target char. Note first `*` _then_ `/`. If we want to search from the end, we use `%/*`. Note that `%` still comes first, just as with `#`, but now, since we are matching from the end, we have `/*` instead of `*/`.



## References

* [Bash Reference Manual - Shell Parameter Expansion](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion)

* [Wooledge Bash Wiki - Parameter Expansion](https://mywiki.wooledge.org/BashGuide/Parameters#Parameter_Expansion)

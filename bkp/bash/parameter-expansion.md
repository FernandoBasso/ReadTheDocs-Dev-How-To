# Parameter Expansion

[TOC]

Read about it in [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion) or `PAGER=less man bash` and then do the search `/^ *Parameter Expansion`.

Parameter expansion is used to perform operations on the value stored in the parameter. Read each appropriate section while trying the examples that follow.

> **NOTE**: These expansions require that the value to be manipulated be stored in a variable.

## ${parameter/pattern/string}

Expand the parameter `sentence`, match the pattern `force` and replace it with the string `source`:

``` shell-session
$ sentence='May the force be with you.'

$ printf '%s\n' "${sentence/force/source}"
May the source be with you.
```

Now compare this with the previous example.

``` shell-session
$ sentence='May the force be with you.'

$ printf '%s\n' "${sentence/force/source/}" 
May the source/ be with you.
```

See the trailing forward slash on “source”? That is one too many forward slashes. It is not part of the expansion and just copied to the output as a literal character.

The curly braces are required, otherwise, we end up just appending text to the original string:

``` shell-session
$ printf '%s\n' "$sentence/force/source/"
May the force be with you./force/source/
```

We just appended “/force/source/” as literal characters to the value stored in `sentence`.

The expansion `${foo/patt/repl}` is not global — it replaces only the first occurrence of `patt`. To replace globally, we use two forward slashes at the beginning of `patt`.

``` shell-session
$ quixote='What your beauty has raised up your deeds have laid low.'

$ echo "${quixote/your/thy}"
What thy beauty has raised up your deeds have laid low.

$ echo "${quixote//your/thy}"
What thy beauty has raised up thy deeds have laid low.
```

In the first version, only the first “your” is replaced with “thy”. In the second, both are replaced.

### “\#” To Match At The Beginning

We can use `#` as the first character in `pattern` to match at the beginning.

``` shell-session
$ str='The Apollo Missions'
$ echo "${str/#Apollo/NASA}"
The Apollo Missions
```

No replacement happens because `Apollo` is not at the beginning. This next example does match and replaces.

``` shell-session
$ str='Apollo Missions'
$ echo "${str/#Apollo/NASA}"
NASA Missions
```

### “%” To Match At The End

We can use `%` as the first character in `pattern` to match at the end.

``` shell-session
$ str='Execute order sixty-six.'
$ echo "${str/%sixty-six/66}"
Execute order sixty-six.
```

No match because there is a dot “.” at the end of the string so “sixty-six” is not really at the end. But this works:

``` shell-session
$ str='Execute order sixty-six.'
$ echo "${str/%sixty-six./66.}"
Execute order 66.
```



## Replace Expansion

We have a string of numbers separated by spaces. Replace each space ` " "` with `" + "`, which would effectively turn the string into a math expression, which could in turn be fed to a program like `bc`.

```shell-session
$ nums='1 2 3 4'

$ echo "${nums// / + }"
1 + 2 + 3 + 4

$ echo "${nums// / + }" | bc
10
```





## The End

Really! This is the end of this file.
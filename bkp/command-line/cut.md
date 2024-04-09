---
description: cut command line examples and tips
---



Intro
-----
Read `man cut`, `info cut` and `cut --help`.

Select specific chars from each input line:

```shell-session
$ cut --bytes=1,3,5,9,13 <<<'May The Force'
MyTFe

$ cut --characters=1,3,5 <<<$'May The\nForce Be With'
MyT
Fre
```


For a range of chars, use the “x-y” notation:

```shell-session
$ cut --characters=1-5 <<<'hello-world'
hello
```



## Field Selection

From `man cut`:

    -f, --fields=LIST
           select only these fields; also print any line that contains no
           delimiter character, unless the -s option is specified
It means if field 3 is selected, and a given line does not contain at least three fields, and it does not contain the delimiter character, then that line is printed nonetheless (unless `-s` is used).

```shell-session
# <1>
$ cut -d \  -f 4 <<<$'hello\nworld\nThe force is strong with this one.'
hello
world
strong

# <2>
$ cut -d \  -f 4 -s <<<$'hello\nworld\nThe force is strong with this one.'
strong
```

In #1, the lines “hello” and “world” were printed despite the fact that those lines contain only one word. Since they do not contain a space, they do not contain the specified delimiter (a SPACE).

In #2, `-s` is used, therefore, lines without the delimiter and without the fourth field are not printed.

**TIP**: Did you see we used “`\`” followed by two spaces? `\SPACE` escapes the space, making it a literal character that _does not split words_. Then, the next SPACE is the word separator. Could have been written as `-d ' '` as well.







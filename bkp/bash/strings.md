# Strings

## Importance of Quoting

> **Caution**
> 
> The “space” character is very important in Bash (and other shells). It delimits _words_, a.k.a _words_.

[Bash man page section on Quoting](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Quoting)

## Basic Examples

Here, `echo` is being given three arguments to print:

``` shell-session
$ echo may the force
```

And here, `printf` is being given one *format specifier* (`%s`) and *three arguments*. The format specifier is reused until all arguments all consumed.

``` shell-session
$ printf '%s\n' be with you.
```

The above is the same as:

``` shell-session
$ printf '%s\n' be
$ printf '%s\n' with
$ printf '%s\n' you.
```

From `help printf`:

> The format is re-used as necessary to consume all of the arguments. If there are fewer arguments than the format requires,  extra format specifications behave as if a zero value or null string, as appropriate, had been supplied.
>
> -- help printf

## Escape Character

### Example 1 - Printing

**Example.**

``` shell-session
$ echo foo\
    bar
foo bar
```

The unquoted escape removes the newline. We are left with `foo␠␠␠␠bar`. That is two arguments. `echo` prints those arguments separated by a single whitespace.


From `help echo`:

> Display the ARGs, separated by a single space character and followed by a newline, on the standard output.
>
> -- help echo


### Example 2 - Assignment

However, the same syntax doesn’t work for an assignment expression:

**Example.**

``` shell-session
$ str=foo\
    bar
```

**Output.**

    $ bash dev.sh
    dev.sh: line 2: bar: command not found

And the reason why [was explained](https://lists.gnu.org/archive/html/help-bash/2019-09/msg00012.html) by no one less than the most honorable Greg Wooledge The First.

In short, it becomes this:

**Example on how it is interpreted.**

``` shell-session
str=foo bar
```

That is, we create an environment variable `str=foo` for the command `bar`. Can be checked with this:

``` shell-session
$ str=foo\
>  printenv str
foo
```

We indeed create an environment variable `str` with the value “foo” for the current command.

## Single vs Double Quotes

### Quoting - Example 1

Note we quote neither the string being assigned to `str` nor the variable passed to `printf`.

``` shell-session
$ str=The force is strong with this one.
-bash: force: command not found
```

The problem is that the shell assigns “The” to `str`, sees the space, which separates token “The” from the token “force” and then tries to run the command `force` using the environment variable `str`, and thus the error.

In short, we are setting the environment variable `str=The` for the command `force`, and the remaining words would become the arguments to the command. Since `force` is not a real command or program (on my system, at least), the error occurs.

With the above, we can then understand why these two next examples produce they output observed.

``` shell-session
$ str=The echo is strong with this one.
is strong with this one.

$ str=The printf '%s\n' is strong with this one.
is
strong
with
this
one.
```

In both cases, we create an environment variable (which is not even used) `str=The`. Then there is the `echo` and `printf` commands. `echo` prints any arguments passed, and `printf`'s format specifier is used as many times as needed to consume all arguments.

### Quoting - Example 2

We can “quote” values either by using single quotes, double quotes, or escaping the spaces and other especial characters. Each of the three strings below is assigned as a single value to the heir corresponding variable:

``` shell-session
$ s1=foo\ bar
$ s2='foo bar'
$ s3="foo bar"
```

In all three cases, the space character is made literal and does not cause the shell to interpret it as a token separator.

So, we try the next example proper:

``` shell-session
$ str='The force is strong with this one.'
$ printf '%s\n' $str 
The
force
is
strong
with
this
one.
```

  - Note we did not quote `$str`. Therefore, the shell does *word-splitting* and `printf` receives several arguments, which it prints according to the format specifier `%s\n` thus, each word on its own line. We made spaces literal when assigning to `str`, but did not prevent word-splitting when using the variable. Double-quoting the variable is the correct approach most of the time:

<!-- end list -->

``` shell-session
$ printf '%s\n' "$str"
The force is strong with this one.
```



## Links and Resources

  - <http://oliviercontant.com/why-is-printf-better-than-echo-in-shell-scripting/>


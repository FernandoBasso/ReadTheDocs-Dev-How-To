# tail

**NOTE**: BusyBox doesn't have the long options for the `tail` program.

Read:

```shell-session
man tail
info tail
tail --help
```



## Last N lines

Last 5 lines, `--lines`, `-n`:

```shell-session
$ tail --lines 5 -
```



## Last N chars

Last 3 chars. Use `--bytes` or `-c`:

```shell-session
$ echo 'foo bar' | tail -c 3 -
ar
<plus a new line here>
```

Prints “ar\n” because `echo` adds a newline to the end, which is the third char.

```shell-session
$ echo -n 'foo bar' | tail -c 3 -
bar
```

Prints “bar” and no newline because we used `echo -n`, so, “r” is the third char.


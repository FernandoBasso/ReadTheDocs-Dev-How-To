# HackerRank Shell - Easy Challenges

Always read the usual `man`, `info` and `--help` for any command.

## first 20 lines text file

Use `--lines` or `-n`.

```shell-session
$ head --lines 20 -
```

See: [challange](https://www.hackerrank.com/challenges/text-processing-head-1).



## first 20 chars of text file

Use `--bytes` or `-c`.

```shell-session
$ head --bytes 20 -
```

See: [challange](https://www.hackerrank.com/challenges/text-processing-head-2).



## middle N lines of text file

```shell-session
$ sed -n '10,20 p'
```

See: [challenge](https://www.hackerrank.com/challenges/text-processing-in-linux---the-middle-of-a-text-file), [sed range addresses](https://www.gnu.org/software/sed/manual/sed.html#Range-Addresses),  `$ info sed 'sed addresses' 'range addresses'`.



## last 20 lines of text file

Use `--lines` or `-n`.

```shell-session
$ tail --lines 20 -
```

See: [challenge](https://www.hackerrank.com/challenges/text-processing-tail-1).



## Last 20 chars of a text file

Use `--bytes` or `-c`.

```shell-session
$ tail --bytes 20 -
```

See: [challenge](https://www.hackerrank.com/challenges/text-processing-tail-2).



## Third char of lines

Use `--bytes` or `-c`

```shell-session
$ cut --bytes 3
```



See: [challenge](https://www.hackerrank.com/challenges/text-processing-cut-1).



## Specific chars of lines

Bytes 1, 5 and 9.

```shell-session
$ echo 'may the force' | cut --bytes=1,5,9 -
mtf
```



See: [challenge](https://www.hackerrank.com/challenges/text-processing-cut-2)



## First N chars of lines

```shell-session
$ cut --bytes=1-4 -
```

See: [challenge](https://www.hackerrank.com/challenges/text-processing-cut-4).



## Print first N fields of tab-delimited input



See: [challenge](https://www.hackerrank.com/challenges/text-processing-cut-5)



## translate [] to ()

```shell-session
$ echo 'for [[j=0; j < 5; ++j]]' | tr '[]' '()'
for ((j=0; j < 5; ++j))
```



## delete lowercase a to z

Use `--delete` or `-d`:

```shell-session
$ echo 'May The Force' | tr --delete 'a-z'
M T F
```




# Numbers on The Command Line

Some tips on how to deal with numbers in the command line.

## Print Numbers from N to M

Using Bash loop:

```bash
for (( i = 1; i <= 9; ++i ))
do
  printf '%s\n' "$i"
done
# Outputs one number per line.
```

NOTE: We can leave a space around the assignment operator like in `i = 1` inside `(( ))` math context to assign a value to a variable.

To print each number on the same line:

```bash
for (( n = 1; n <= 9; ++n ))
do
  echo -n "$n"
done
```

```shell-session
$ bash script.sh
123456789
```

Add a space manually with `echo -n "$n "` or `printf '%s ' "$n"` to separate the numbers. The downside is that the last number will still contain a trailing space unless handled with a conditional or something similar.

Using `printf` and a range, one number per line:

```shell-session
$ printf '%s\n' {1..9}
```

To produce the output in a single line:

```shell-session
$ printf '%s ' {1..9}
```

Again, there is a trailing space after the last number.

Using `echo` and a range outputs each number on a single line, because echo prints each argument followed by a newline:

```shell-session
$ echo {1..9}
```

With `-n`, echo _does not_ print a newline after each argument. Moreover, `echo` automatically outputs each of its arguments separated by a (single) space:

```shell-session
$ echo -n {1..9}
```

So, this version doesn't have the trailing space after the last number! Great!

Or using `seq`. To print from increments of one, only two arguments are required, the first and last numbers in the sequence:

```shell-session
$ seq 1 9
```

The default separator is `\n`, but `seq` can handle others:

```shell-session
$ seq --separator='_-_' 1 9
1_-_2_-_3_-_4_-_5_-_6_-_7_-_8_-_9

$ seq -s '\t' 1 9
1\t2\t3\t4\t5\t6\t7\t8\t9

$ seq -s $'\t' 1 9
1	2	3	4	5	6	7	8	9
```

Note the `$''` bash syntax.



## Print Odd Numbers From N to M

Using a `for` loop:

```bash
for (( n = 1; n <= 9; ++n ))
do
  if (( n % 2 == 0))
  then
    continue
  fi
  printf '%d ' "$n"
done
```

```shell-session
$ bash script.sh
1 3 5 7 9
```



Using a range, we pass START, END, and STEP:

```shell-session
$ echo -n {1..9..2}
```

With `seq`, we now pass three parameter, the START, the STEP, and END:

```shell-session
$ seq -s ' ' 1 2 9
1 3 5 7 9
```


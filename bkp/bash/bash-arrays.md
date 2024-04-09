---
description: Examples and tips on using arrays in Bash
---

# Introduction to Bash Arrays

[TOC]

## Create And Use An Array

Create an array of numbers. Use `()`, and, unlike in many other languages, separate elements with spaces, not commas:

```shell-session
$ nums=(1 2 3 4)
```

Use the elements of the array in a loop:

```shell-session
$ for n in "${nums[@]}" ; do printf '  %s\n' "Iteration $n" ; done
  Iteration 1
  Iteration 2
  Iteration 3
  Iteration 4
```

Get the length of the array:

```shell-session
$ echo "${#nums[@]}"
4
```

Append elements to the array:

```shell-session
$ nums+=(5 6)

$ echo "${#nums[@]}"
6

$ echo "${nums[@]}"
1 2 3 4 5 6
```

# Array Subscript asterisk \* and at @

`man bash` ([man bash on arrays](https://www.gnu.org/software/bash/manual/bash.html#Arrays)):

> Any element of an array may be referenced using `${name[*]}`. If the subscript is `@` or `*`, the word expands to all members of the array name. These subscripts differ only when the word appears within double quotes. If the word is double-quoted, `${name[*]}` expands to a single word with the value of each array member separated by the first character of the IFS variable, and `${name[@]}` expands each element of name to a separate word.
> 
> —  man bash 

By default the value of `IFS` is a space, a tab, and a newline. Let’s apply what we just discussed to convert an array to a string.

First, create an array with four letters, then, to see the difference between `[*]` and `[@]`, let’s use `printf '%s\n'`, since the specifier `%s` is reused as necessary to consume all given parameters:

```shell-session
$ arr=(a b c d)

# <1>
$ printf '“%s”\n' "${arr[*]}"
“a b c d”

# <2>
$ printf '“%s”\n' "${arr[@]}"
“a”
“b”
“c”
“d”
```

<1> Because `[*]` is used, the array is expanded to a single element, thus `“%s”\n` is used only once, and the entire output is a single line inside the curly double quotes.

<2> Because `[@]` is used, each element of the array is expanded to a separate word, thus causing `“%s”\n` to be used four times, producing the four output lines.

> **NOTE**: Using `[*]` it is possible to actually convert an array into a string.

Let’s see what happens when we change the value of `IFS`:

```shell-session
$ IFS='#'; printf '“%s”\n' "${nums[*]}"
“1#2#3#4”

$ IFS='_'; printf '“%s”\n' "${nums[*]}"
“1_2_3_4”

$ IFS=$'\t'; printf '“%s”\n' "${nums[*]}"
“1   2    3    4”

$ IFS=$'\n'; printf '“%s”\n' "${nums[*]}"
“1
2
3
4”
```

# Convert Array To String

We can get an array and turn it into a string with the elements separated by spaces. First, let’s create an array of numbers:

```shell-session
$ nums=(1 2 3 4)
```

Get the length of the array:

```shell-session
$ echo "${#nums}"
4
```

Use the `[*]` syntax on the double-quoted name of the array to turn it into a string:

```shell-session
$ strnums="${nums[*]}"
```

The resulting value now has length 7, because our string consists of four digits and three spaces.

```shell-session
$ echo "${#strnums}"
7
```

Shows that it loops only once, printing the entire string in a single run:

```shell-session
$ for x in "${strnums[@]}" ; do echo "“$x”" ; done
“1 2 3 4”
```

Now that `strnums` is a string, even using `[*]` to loop produces a single word:

```shell-session
$ for x in "${strnums[*]}" ; do echo "“$x”" ; done
“1 2 3 4”
```

# read and IFS

From `help read`:

> Read a line from the standard input and split it into fields.
> 
> Reads a single line from the standard input, or from file descriptor FD if the -u option is supplied. The line is split into fields as with word splitting, and the first word is assigned to the first NAME, the second word to the second NAME, and so on, with any leftover words assigned to the last NAME. Only the characters found in $IFS are recognized as word delimiters.
> 
> —  help read 

On with the examples and explanations.

### Example 1

Since the `-a` option was not used for `read`, `IFS='|'` has no effect, and the entire input line is assigned as a single word to `line`.

```shell-session
$ while IFS='|' read -r line ; do printf '%s\n' "$line" ; done <<<'a|b|c'
a|b|c
```

### Example 2

This time `-a` is used, but only one NAME is provided, therefore, all the words are “assigned to the last NAME”. However, since simply `"$line"` is used as the parameter to `printf`, it results in the first element of the array being retrieved (not all of them). It is similar to array pointers in C, where the name of the arrays is a pointer to the first element of the array.

```shell-session
$ while IFS='|' read -r -a line ; do printf '%s\n' "$line" ; done <<<'a|b|c'
a
```

### Example 3

Because `"${line[*]}"` is used (instead of `"${line[@]}"`), the elements were expanded to a single word, and each element is separated by the first character of IFS, which is a space by default. This effectively joins the array elements into a single string, with spaces as the separator.

```shell-session
$ while IFS='|' read -r -a line ; do printf '%s\n' "${line[@]}" ; done <<<'a|b|c'
a
b
c
```

### Example 4

`read` reads *a single line* from the input source (`help read`). That is why this example does not loop twice and assign 'x', 'y', and 'z' in turn to `line`. Rather, it loops only once and assigns the entire input to `line`.

```shell-session
$ while IFS='|' read -r -a line ; do printf '%s\n' "${line[*]}" ; done <<<'a|b|c'
a b c
```

> **Tip**
> 
> Note we are quoting `|`, otherwise it would create a pipe and would not behave correctly. Remember that the two ways of quoting especial characters is by surrounding it with single quotes, or prepending it with a backslash.

## convert each character on a string to an array element

https://stackoverflow.com/questions/7578930/bash-split-string-into-character-array

geirha's version

```shell-session
'''
var=abc;
for (( i = 0, n = ${#var}; i < n; ++i )); do
    arr+=( "${var:i:1}" )
done
```

davidl_'s version

```shell-session
while read -r -n1  ; do
    chars_array+=($a)
done <<<"$chars_string"
```

```shell-session
echo "abcdefg" | fold -w1

echo "abcdefg" | grep -o .
```

# Links and Resources

- [Bash Manual: Arrays](https://www.gnu.org/software/bash/manual/bash.html#Arrays)

- [Bash Manual: Special Parameters](https://www.gnu.org/software/bash/manual/bash.html#Special-Parameters)

- [Bash Manual: Word Splitting](https://www.gnu.org/software/bash/manual/bash.html#Word-Splitting)

- [Unix StackExchange Question About IFS](https://unix.stackexchange.com/questions/26784/understanding-ifs)

### Hello

Hello
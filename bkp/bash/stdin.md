---
title: Bash STDIN Examples and Tips
---

To provide input and parameters to scripts, we can use `read` and:

  - `/dev/stdin`

  - `<&0`

# /dev/stdin

`/dev/stdin` doesn’t play well with `<<<$'10\n20'`.

Taking this script, see two possible ways to feed input to it and the output each produce:

``` bash
read -r x < /dev/stdin
read -r y < /dev/stdin

echo "$x"
echo "$y"
```

Output for `/dev/stdin` and Here String:

``` shell-session
$ bash dev.sh <<<$'10\n20'
10
10
```

Output for `/dev/stdin` and normal input:

``` shell-session
$ bash dev.sh
10 (typed manually)
20 (typed manually)
10
20
```

With the Here String approach, both `x` and `y` are assigned 10, which is not what we wanted. With the normal input, `x` is assigned 10, and `y` is assigned 20, which is what we expected.

Let’s see with a different way of reading from STDIN.

# Redirect <&0

``` bash
read -r x <&0
read -r y <&0

echo "$x"
echo "$y"
```

``` shell-session
$ bash dev.sh <<<$'10\n20'
10
20

$ bash dev.sh
10 (typed manually)
20 (typed manually)
10
20
```


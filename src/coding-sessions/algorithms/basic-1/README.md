# Basic 1

## Running Tests

```shell-session
$ npm install --save-dev jest
$ npx jest path/to/file.spec.js

$ npx jest basic-1/add.spec.js
```

## add(x, y)

Write a function that adds two numbers.

Examples:

- 0 + 0 = 0
- 1 + 1 = 2
- 10 + 1 = 11
- -1 + -1 = -2
- -1 + 1 = 0
- 1 + -1 = 0

# sub(x, y)

Write a function that subtracts `y` from `x`.

Examples:

- `sub(10, 1)` -> 9
- `sub(1, 10)` -> -9
- `sub(0, 0)` -> 0

# isEven(x)

Write function that checks whether a number is even.

Examples:

- `isEven(2)` -> `true`
- `isEven(-3)` -> `false`

# isOdd(x)

Write function that checks whether a number is even.

Examples:

- `isEven(2)` -> `false`
- `isEven(-3)` -> `true`

## TODO

JavaScript has types.

- Dymanically Typed Languages (types are defined at runtime).
- Statically Typed Languages (types are defined at compile time).
- Strongly Typed Languages (no implicit type conversion).
- Weakly Typed Languages (implicit type conversion).

------------------------------------------------------------------------

JS: `1 + '1'`.

Python `1 + '1'` vs `1 + str(1)`.

int num = 1
Java: `System.out.printnl("Number: " + 1)`

Python:
```text
>>> x = 1

>>> type(x)
<class 'int'>

>>> 1 + '1'
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unsupported operand type(s) for +: 'int' and 'str'
>>> str(1) + '1'
'11'
>>> 1 + int('1')
2
>>>
```

JavaScript:

```text
> 3 + '3'
'33'
> 3 * '3'
9
```

Fail Silently:

```text
> xs = [1, 2]
[ 1, 2 ]
> xs[0]
1
> xs[0] = 10
10
> xs
[ 10, 2 ]
> 
> 
> s = 'fello'
'fello'
> 
> s[0]
'f'
> 
> s[0] = 'h'
'h'
> 
> s
'fello'
> 
```

------------------------------------------------------------------------

`toBeFalsy()` and `toBeTruthy()` and truthy and falsy values in JavaScript.


```
== and != does type conversion BEFORE comparing.

'1' == 1 -> true

=== and !== does NOT do type conversion.

'1' === 1 -> false
```

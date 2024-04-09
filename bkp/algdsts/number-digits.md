---
title: Operations on Digits of a Number
description: Ideas relating to extract digits from a number, like the first or last digit, etc. Includes some interesting math and division concepts.
---

# Operations on Digits of a Number

Some concepts, notes, tips and examples on how to extract digits from a number.
For example:

- Count digits in a number.
- Extract first and last digits.
- Extract first n digits.
- Extract last n digits.
- Turn a number into an array of its digits.
- What else?

Most of it apply only to integers, or whole numbers (positive integers).

Some solutions on the web involve converting the number to a string, slicing it to get the desired digits, and then converting the sliced parts back to a number.
That is fine but there are also, sometimes, some more mathematical approaches to the problem (which would also be more performant).

## Notes on rounding down to integer

Rounding a number down to an integer can be done with `floor`-like functions in many languages, or with a bitwise operation.

```
$ node --interactive

> var n = 794;

> while (n >= 10) n /= 10;
7.94

> n
7.94

> n ^ 0
7
```

We could also do `n | 0` or `~~n`.
And of course, `Math.trunc()` and `Math.floor()`.

I know PureScript uses `n | 0` (saw it in the source code).
[@natefaubion PureScript Discord server](https://discord.com/channels/864614189094928394/865617619464749081/1015376935485968474) told me “it will wrap to an int32 range” and that “it was an old trick from asm.js.”

```js
//
// .spago/prelude/v6.0.0/src/Data/Ring.js
//
export const intSub = function (x) {
  return function (y) {
    return x - y | 0;
  };
};
```

Bitwise operators in ECMAScript convert the number operands to int32, which make the o the result is an integer value.
That is why bitwise operations on ECMAScript numbers return numbers without the fractional part.

> A bitwise operator treats their operands as a set of 32 bits (zeros and ones), rather than as decimal, hexadecimal, or octal numbers.
> Bitwise operators perform their operations on such binary representations, but they return standard JavaScript numerical values.
>
> — [MDN docs on bitwise operators](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#bitwise_operators)

- [ECMAScript spec on bitwise operations](https://tc39.es/ecma262/#sec-numberbitwiseop).
- [MDN Docs on bitwise oprators](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#bitwise_operators).

## Count digits in a number

To count the digits in a number, *log base 10* is helpful.
For example, `log10(100)` is 2 and `log10(1000)` is 3.
Note we get one less than the actual number of digits in each case.

Similarly, `log10(99)` is ≃ 1.9 and `log10(199)` is ≃ 2.9.
If we round down to the integer part, we again get 1 and 2, which is again one less than the number of digits for each number.
Therefore, we can simply add 1 and we get the count of digits in a number.

```
$ node --interactive

> log10(100) + 1
3

> log10(1000) + 1
4

> floor(log10(99)) + 1
2

> log10(99)
1.99563519459755

> floor(log10(99)) + 1
2
```

We just floor the result of the `log10` and add 1 to that and we are done.
And we have to remember that logarithms with 0 yields `Infinity`, while logarithms with negative integers yield `NaN`, so, we can just handle the 0 case conditionally, and get the absolute value for the given number before performing the logarithm operation.

### JavaScript

For this solution we use the `OR` (`|`) bitwise operation with 0 (instead of the `floor` function, which we could just as well) to “remove” the decimal part.

```
--8<-- "docs/algorithms/number-digits/countDigits.js"
```

## Turn number into array of its digits

Using the modulo operation to keep getting the last digit and adding it to the **front** of the array.

- 1. Let `digits` be an empty array.
- 2. While `n >= 10`:
   - a. Let `m` be the result of `n` module `10`.
   - b. Let `n` be the result of flooring `n` divided by `10`
   - c. Add `m` to the front of `digits`.
- 3. Add floored `n` to the front of `digits`.
- 4. Return `digits`.

For 793, this is how it goes:

```text
793 % 10      -> 3
793 / 10 | 0  -> 79
digits is [3]
           ^

79 % 10       -> 9
79 / 10 | 0   -> 7
(note we need to add 9 *before* 3 in digits)
[9, 3]
 ^

Add remaining 7 in front of [9, 3]
[7, 9, 3]
 ^
```

At each iteration of the loop, `n` is relieved of its last digit, and `digits` gets that digit added to is beginning.

### JavaScript

```js
--8<-- "docs/algorithms/number-digits/numToDigits.js"
```

## Take first digit from number

Keep dividing the number by 10 while the number is greater than 10 and then, if there are decimal places left, apply an operation to round it down to the nearest integer.

### JavaScript

```js
--8<-- "docs/algorithms/number-digits/getFirstDigit.js"
```

## Take last digit from number

To get the last digit of an integer, simply do modulo division by 10.

### JavaScript

```
$ node --interactive

> 1984 % 10
4
> -1984
-1984
> (1e3 + 7) % 10
7
```

No matter the length of the number, it always works.
No loop or conversion to string with some split is necessary.

```js
--8<-- "docs/algorithms/number-digits/getLastDigit.js"
```

## Take first n digits from number

If we have 12345, and we keep dividing it by 10 and flooring the result, we keep “dropping” the last digit:

```
$ node --interactive

> var n = 12345;

> n / 10
1234.5

> n / 10 | 0
1234

> n / 10 / 10 | 0
123

> n / 10 / 10 / 10 | 0
12

> n / 10 / 10 / 10 / 10 | 0
1
```

If we want to get the first three digits, we have to “drop” the last two.
Or, we have to divide by 10 two times, which is the same as dividing by (10 * 10), which is 10 to the second power.

We can do a loop, something like:

```js
var n = 12345;
while (countDigits(n) > 3)
  n = n / 10 | 0
// → 12
```

Or

```js
var n = 12345;
for (var i = 0; i < 5 - 2; ++i)
  n = n / 10 | 0;
// → 12
```

Then we can think of this logic: “to get the first *n* digits, we need to drop the last *m* digits.”
If the number has five digits, and we want the first three, 5 - 3 is 2.
We need to drop the last two digits.
And we know that “dropping the last two digits” means dividing by 10 two times, or by `10 / pow(10, 2)`.

```
$ node --interactive

> n / pow(10, 5 - 1) | 0
1

> n / pow(10, 5 - 2) | 0
12

> n / pow(10, 5 - 3) | 0
123

> n / pow(10, 5 - 4) | 0
1234

> n / pow(10, 5 - 5) | 0
12345
```

### JavaScript

```js
--8<-- "docs/algorithms/number-digits/takeDigits.js"
```

## Drop first n digits from number

For this we can make use of powers of 10 mixed with taking the last digit in a loop.

```text
num = 7953

last = 7953 % 10             -> 3
num  = 7953 / 10 | 0         -> 795
out  = 3 * 10 ** 0            -> 3

last = 795 % 10              -> 5
num  = 795 / 10 | 0          -> 79
out  = 5 * 10 ** 1 + out     -> 53
```

- `n % 10` returns the last digit in `n`.
- `n / 10 | 0` returns `n` without the last digit.
- `x * 10 ** exp` makes use of the knowledge that we use a positional numeric system.
  `digit * 10 ** 0` for the one's place, `digit * 10 ** 1` for the ten's place, `digit * 10 ** 2` for the hundred's place, etc.
  For example:
  - `7 * 10 ** 0` is 7.
  - `7 * 10 ** 1` is 70.
  - `7 * 10 ** 2` is 700.

### JavaScript

```js
--8<-- "docs/algorithms/number-digits/dropDigits.js"
```

## References

- [How do I determine the number of digits of an integer in C? (StackOverflow)](https://stackoverflow.com/questions/1068849/how-do-i-determine-the-number-of-digits-of-an-integer-in-c).
- [How To Get First N Digits Of A Number](https://www.c-sharpcorner.com/blogs/how-to-get-first-n-digits-of-a-number).

---
title: JavaScript Types and Conversions
---

# JavaScript Types and Conversions

[TOC]

## Undefined

### Casting

Casting `undefined` to a number produces `NaN`:

```node-session
> Number(undefined)
NaN
> parseInt(undefined, 10)
NaN
> parseInt(10 / 3 - undefined)
NaN
```

@TODO: Research on casting vs coercion.

### Arithmetic Operations

In a numeric/arithmetic context, `undefined` is toxic! Any arithmetic operation involving `undefined` produces `NaN`.

```node-session
> undefined + 1
NaN
> undefined - 1
NaN
> undefined * 1
NaN
> undefined ** 1
NaN
> undefined / 1
NaN
> undefined % 1
NaN
```

## Null

### Casting

`Number` and the unary plus operator produce 0:

```node-session
> Number(null)
0
> +null
0
```

`parseInt` produces `NaN`:

```node-session
> parseInt(null, 10)
NaN
```

### Arithmetic Operations

In arithmetic operations with `null` and a number, `null` is (as seen above) converted to zero:

```node-session
> null - 1
-1
> null * 5
0
> (null + 6) / 4
1.5
```

In all three cases above, `null` becomes zero, and thus the outputs.

## True and False

### Casting

`Number()` and the unary plus operator produce 0 for `false` and 1 for `true`.

```node-session
> Number(false)
0
> Number(true)
1

> +false
0
> +true
1
```

`parseInt()` produces `NaN` for both.

```node-session
> parseInt(false, 10)
NaN
> parseInt(true, 10)
NaN
```

So, this is 0 because casting a non-empty string to a boolean produces `true`, negating it produces `false`, and `false` as a number is 0:

```node-session
> +!'Lara Croft'
0
> Number(!'The Angel Of The Darknes')
0
```

And by the same logic, no wonder this produces 1, because casting a non-empty string to a boolean produces `true`, negating it produces `false`, negating that again produces `true`, and `true` as a number is 1:

```node-session
> +!!'Tomb Raider'
1
> Number(!!'The Last Revelation')
1
```

### Arithmetic Operations

Remember: `false` becomes 0, `true` becomes 1.

```node-session
> Number(false) - 0
0
> Number(false) - 1
-1
> +false * 3
0
> +false * -3
-0
> Number(true) / 2
0.5
> +true / 4
0.25
```

## Plus operator “+”

The `+` operator is overloaded to do both addition of numbers and concatenation of strings. If both operands are numbers, it adds the values. If both are strings, it concatenates them. If one operand is a string and the other is a number, it first converts the number to a string and then concatenates the strings.

```node-session
> 1 + 1
2

> '1' + '1'
'11'

> '1' + 1
'11'

> 23 + '4'
'234'
```

Note that when handling a numeric string and a number, or even only numeric string operands, all other operators (`-`, `*`, `**`, `/` and `%`) convert the numeric strings to numbers, and then does normal arithmetic operations on them:

```node-session
> '3' - 2
1

> '3' ** '3'
27

> '3' - '2'
1

> '3' * '3'
9

> 10 / '3'
3.3333333333333335

> '8' % '3'
2
```

## Conversion, Coercion, Casting

**Cast** implies an _explicit_ type conversion. **Coercion** implies an _implicit_ type conversion.

> Type coercion is the automatic or implicit conversion of values from one data type to another (such as strings to numbers). *[Type conversion](https://developer.mozilla.org/en-US/docs/Glossary/Type_conversion)* is similar to *type coercion* because they both convert values from one data type to another with one key difference — *type coercion* is implicit whereas *type conversion* can be either implicit *or* explicit.

Examples of coercions (implicit):

```node-session
> 5 + ' remaining'
'5 remaining'

> '2' * 5
10
```

In the first case, 5 is first _implicitly_ converted to a string, and then concatenated with the other string. In the second case, the string `'2'` is first converted to the number 2, and then multiplied by 5.

Examples of casting (explicit):

```node-session
> 2 + Number('2')
4

> +false - 5
-5
```

## Links and Resources

- [Type Conversion -- MDN](https://developer.mozilla.org/en-US/docs/Glossary/Type_conversion)
- [Type Coercion -- MDN](https://developer.mozilla.org/en-US/docs/Glossary/Type_coercion)
- [Comparison Operators -- MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Comparison_Operators)
- [Type Conversion -- Wikibooks](https://en.wikibooks.org/wiki/Computer_Programming/Type_conversion)
- [Type conversion -- Wikipedia](https://en.wikipedia.org/wiki/Type_conversion)
- [Cast vs Coercion -- Stackoverflow](https://stackoverflow.com/questions/8857763/what-is-the-difference-between-casting-and-coercing)
- [undefined -- MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/undefined)

## The End

---
title: Tuples | TypeScript
description: Notes, tips and examples on Tuples in TypeScript.
---

# Tuples

## Introduction

From the [docs,window=_blank](https://www.typescriptlang.org/docs/handbook/2/objects.html#tuple-types):

> A _tuple type_ is another sort of `Array` type that knows exactly how many elements it contains, and exactly which types it contains at specific positions.

Arrays require elements of a single type and the length is unknown.
Tuples allow for elements of different types, and the length is known.
The docs don’t mention it, but it means and **the length is implicitly part of the type of the tuple**, which is why trying to access an index position outside the range of the type is an error caught by the type checker.

Declare a tuple:

```typescript
const jedi: [string, number] = ["Yoda", 900];
//          ----------------
//                 \
//                  \
//                   v
//           type of the tuple
```

Now it is possible to access the elements and each position is of a known type:

```typescript
const name: string = jedi[0];
const age: number = jedi[1]
```

Or destructuring:

```typescript
const [name, age] = jedi;
```

And `name` and `age` are of type `string` and `number` respectively.
But these next ones cause an error: <q>Tuple type '[string, number]' of length '2' has no element at index '2'.</q>

```typescript
const skill = jedi[2];
//                  \--> Error trying to access
//                       index 2 of tuple with
//                       indexes 0 and 1.

const [, , jediSkill] = jedi;
//    0  1     2
//              \
//               \
//                v
//         Same as above.
```

If a tuple (or array for that matter) contains two elements, its indexes are 0 and 1 (not 1 and 2).

An array type would not cause a type error error on nonexistent indexes because for arrays the length is not known and not part of the type.

* TypeScript Playground: [Tuple Example](https://www.typescriptlang.org/play?removeComments=true&jsx=0&module=1&pretty=true&preserveWatchOutput=false&inlineSourceMap=false#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMCFBMmEAKJkAloggo8xwwBtTpMhHTxCERXNOJKtPEZMQOxAOYuLPG1hPF4MU2FUVABrAFtidGj9Ql9xLQAKXntVfUUIdIBKCABvUAgIaQSzREsIAF4IACIAFXgOWp0AD2JY9tNMbGaATw48AGU5dH0OREaAbjKICjl4WLwlcURA+urLBYBfQoKF0AVCWiNzVF9sAGFFBksAbUar30aAXR2zx7xxN-EwmSZHSP1QlmOmh+RgAVngDNgnhcpikADQQQirELoL4NF4ATVQZGIjXRAE4AAwUj4LSDlekMgC0zJZrLZjO0DK53IgAB1OTzBXyBUKedIRTzECNTKheMZkC42pYoQ8jCQ1thkclfDs4QYntSFtCIH48NhMbFsbr4fongBGGmnVUm-wAdXQil8w1GmsQKJ1DT1todtOYAD8IN7TAByC3Y6MQfS0DGoIxifS+EjCWquYzSiDRrUpaNpbQAETwyPgqng-pN5gYEAA7hhorQ0sankGAHI9PDooOwfy4iBBk7G2hJczma36gBMjsgEdafTzowLSL92vRcYIHwTsoglhSKALc+j2gjyDoKYgeEsayUJqMwLwnTPJZV5yMT3RA5tYxTuYI5jpekZKi4+bRpu-o7lie4HnKx6+Ke0bnmB17JoQlD3ngj5pi+5Bvh+pYaJAzQKsYEEQDB24ungXyYY4hDmEMxgtom5D6M43AQBSJrkBAdppJAACSL7Jvo054L4xAzrmfpsbmKg8cmpBsamCroJxUjvigxASdo2FGPx1hCSJYCgG+HAYEYxT7HMQA).

## Type Inference

Many books, tutorials, and even the docs like to recommend relying on type inference in TypeScript by default and only use explicit type annotations when strictly required.
I recommend the opposite: By default almost always write explicit type annotations and only occasionally, on some special circumstances rely on type inference (TODO: Write about this more thoroughly in some other document and link here).

If we attempt to write a tuple and let type inference determine the type, it actually infers array types:

```typescript
const yoda = ["Yoda", 900];
```

The type of `yoda` is `(string | number)[]`, not `[string, number]`.

Consider this:

```typescript
const name = yoda[0];
```

What is the type of `name`?
It is `string | number`.
What about this next snippet?

```typescript
const [name, age] = jedi;
```

What are the types of `name` and `age` above?
Again, both are of type `(string | number)[]`.

So yep, we need explicit types for tuples.

* TypeScript Playground: [Tuple type inference as arrays](https://www.typescriptlang.org/play?removeComments=true&jsx=0&module=1&pretty=true&preserveWatchOutput=false&inlineSourceMap=false#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMCFBMmEAKJkAloggo8xwwBtTpMhHTxCERXNOJKtPEZMQOxAOYuLPG1hPF4MU2FUVABrAFtidGj9Ql9xLQAKXntVfUUIdIBKCABvUAgIaQSzREsIAF4IACIAFXgOWuaATw5TAElCfnQ8QmcITGwunoBlOXR9DkRGgG4yiAo5eFjhxHFEQPrqyxWAX0KCldAFQlojc1RfbABhRQZLAG1Gu99GgF0Dq9eeHEX3EwmSZHSANQlnOmgBRk6qDIxAOHwAmkjiI0ADQQACcAAYCT8VhpIJNTPpaPkbnMUhAAD4QQibELoApvH64wioIxvWnJXzc1kEH5pMnMZrIYhGKm4ryIbqU6mkCAJdDETqOXgQPCGZAECAClK0bQYZki9C0cUgMCXF58khbXF+PB-BqI5EreHMg6e4hvYmk9TMfq0HqqYwGxU9amoHVOvAu-xq8jMm0QKV4LUJKzmcxGxB03yMi2xNk2zR4AAeHAwRmKxyWQA).

## Labeling elements

Tuples allow labeling the elements.

Consider this:

```typescript
type Jedi = [number, string];
```

It is not so clear what the first number element is about.
Is it an ID, or age, or skill level in percentage form?
Same for the second element.
Is the string supposed to be used for name of the jedi?

Fortunately, it is possible to label the elements of a tuple so tolling and editors can offer better feedback, while also providing more information for people simply reading the code:

```typescript
type Jedi = [id: number, name: string];
type Padawan = [name: string, age: number, level: number];
```

## Tuples in function arguments

Function arguments can be described with tuples.

```typescript
type Jedi = [name: string, level: number];

const yoda: Jedi = ["Yoda", 100];

function print(...args: Jedi): void {
  log(args[0], args[1]);
}
```

## Variadic Tuples

A variadic tuple has length and type of elements, like <q>normal tuples</q>, except that _the exact shape is yet to be defined_.

* [Variadic Tuples documentation,window=_blank](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-0.html#variadic-tuple-types).

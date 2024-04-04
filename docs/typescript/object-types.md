---
title: Object Types | TypeScript
description: Notes, tips and examples on object types in TypeScript.
---

# Object Types

## Introduction

TypeScript offers three object types:

* `{}`, which can be called the _anything that is non-null and non-undefined type_.
TODO: Find the official docs on that type.
* `object`
* `Object`

Because these types are very generic, using more specific types is recommended.
That is, instead of, for example, `object`, try to use a type which specify the properties more precisely.

We should almost never use `{}` or `Object`.
`object` is sometimes acceptable when no other more specific type seems to make sense.

`{}` is a _ban-type_ in default eslint.
Check [ban-types on ESLint documentation,window=_blank](https://typescript-eslint.io/rules/ban-types/).

## Non-null and non-undefined type {}

The _non-null and non-undefined type `{}` allows any value of any type (literal or compound) as long the values are not `null` or `undefined`.

These are all valid:

```typescript
var x: {} = 1;
var y: {} = "TS";
var f: {} = function fn() { return 1; };
var v: {} = {};
var w: {} = { lang: "TypeScript" };
var a: {} = [];
var b: {} = [1, "two", { three: 0b0011 }];
// etc.
```

But this is illegal:

```typescript
var n: {} = null;
// ~ Type 'null' is not assignable to type '{}'.

var u: {} = undefined;
// ~ Type 'undefined' is not assignable to type '{}'.
```

The type `{}` accepts the set of all values of all types (not only compound types, but also literal types) except `null` and `undefined`.

## object type

The `object` type (with lowercase “o”) allows for values of _compound types_ (but no literals).
Compound types include arrays, maps, sets, functions, classes or any value which is not a primitive.

These are all valid:

```typescript
var v: object;
v = {};
v = { name: "Aayla Secura" };
v = function f() {};
v = class Lara {};
v = [];
v = [1, "two", { three: 0b0011 }];
v = new Set();
v = new Map();
v = () => undefined;
v = /regexp/i;
```

* [TypeScript Playground: object type valid values,window=_blank](https://www.typescriptlang.org/play?#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMCFBMmEAErxCEFAEtaEYXl4Y8Z1KgDWAW2Lo7RwgHNxWgBS99qo0UIbwBKCABvUAgIaWdDI0QAG2sAXggAchEAKzxVQwBPDmtYxKMyGOJE+DxubAAVQrwAZTl0Iw5EdKiICjl4BzwlcUQE5Ig0kaS8AG5QAF9QkNnQBUJaRAhE1HdsAGFFBmSAbQAiLfcTgF1xiFXDvHFz8WE3Mm871GSlzVj0GOxsrlELNpDdwnMQWCICQBtgTrBiPlEsQIE1cvB0MQThAIaBQWk-IQAkFeKEIrj8bdkfQIAAZZwo8GQtJHS7MiBHACMABoICdEAB3VAnXnhQzIdB4PDYAAMwhlMs5nJxbLxN0IeAFqLwiFC7I1WoAssQOHq1WkySkAHwQfRSXhuPBkdnASXuPAADw4wCMsyAA).

Remember that functions have `Object` as their `prototype`, so it should not come as a surprise that a function can be assigned to an identifier of type `object`.

But these produce type errors:

```typescript
var a: object = undefined;
var b: object = null;
var c: object = 0xff;
var d: object = "two";
var e: object = Symbol("a-symbol");
var f: object = false;
```

* [TypeScript Playground: object type invalid values,window=_blank](https://www.typescriptlang.org/play?#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMCFBMmEAErxCEFAEtaEYXl4Y8Z1KgDWAW2Lo7RwgHNxWgBS99qo0UIbwBKCABvUAgIaWdDI0QAG2sAXggAchEAKzxVQwBPDms3WMSjMhjiRPg8bmwAFUK8AGU5dCMORHSoiAo5eAc8JXFEBOSINNGkvABuUABfUJC50AVCWkQIRNR3bABhRQZkgG0AIm33U4BdCYg1o7xxC-FhNzJve9Rk5c1Y9AhiNhsrlNml9FJeG48GQ5n8zEDhDk8mlCPBEolYXE5AikaCIAAGAAevF4mP+ZBxINup0QAHdUKcyRA8JTkRBmvkHMIvt5TsQALS0TncxKnH5w3isvG8Kq0WZAA)

## Object type

`Object` used as a type allows for all values whose prototype is `Object`, which means every primitive _and_ compound types.

Because `Object` is an interface in TypeScript, it requires a `toString()` method which returns some string.

```ts
var o: Object = {
  toString() {
    return null;
    // ~ Type '() => null' is not assignable to
    // ~ type '() => string'.
  },
};
```

Our `toString()` is returning a value which is not a string, therefore, it doesn’t satisfy the `Object` interface and a type error is produced.
If we return any string from that method, then it type-checks.

* [TypeScript Playground: Object type wrong toString(),window=_blank](https://www.typescriptlang.org/play?#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMCFBMmEAErxCEFAEtaEYXl4Y8Z1KgDWAW2Lo7RwgHNxWgBS99qo0UIbwBKCABvUAgIaWdDI0QAG2sAXggAcgB5YQArPFVDAE8Oa0RUAGVEdDd3UIhMbAAVYrxyuWqORHSoiAo5eAc8JXFEBOSINNGkvABuUABfUJC50AVCWkQIRNR3bABhRQZkgG0AIm33U4BdCYg1o7xxC-FhNzJve9Rk5c1Y9AhUNhsnkCmlwvM5pQwT0QBpopAABJ0QyUdB4RDwIgQYgQDbVDwAGjMSAgAHdrM5rGiMUQajDmIR4IlEkSGBAEr1UHhaIR0ptaMRRrReIVDMhrAADYH5RAS9lKAi8FSPek9MqVfG1MKRaLRamYgzpdJzaLzAkLGZAA)

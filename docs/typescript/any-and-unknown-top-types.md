---
title: any and unknown Top Types
description: Notes, tips and examples on any and unknown TypeScript top types.
---

# Any and Unknown Top Types

## Introduction

TypeScript has two so-called _top types_: `any` and `unknown`.
`any` essentially disables the type system, while `unknown` allows one to be _cautious_.

A _top type_ is a type compatible with any value of (almost) any type.
That is, any values whatsoever can be assigned to `any` and `unknown`:

```typescript
const w: any = 1;
const x: any = { two: 2 };
const y: unknown = "three";
const z: unknown = ["four", 5, { six: 6 }];
```

Even `null` and `undefined` can be assigned to `any` and `unknown`:

```typescript
const h: any = null;
const i: any = undefined;
const j: unknown = null;
const l: unknown = undefined;
```

While `any` allows one to do everything with a value, `unknown` allows nothing.
Read on.

## any

For these examples, use `strict` mode or at least set `noImplicitAny` in `tsconfig.json`:

```json
{
  "compilerOptions": {
    "strict": true
  }
}
```

Or at least:

```json
{
  "compilerOptions": {
    "noImplicitAny": true
  }
}
```

Every value is compatible with `any`.

Every sub-type except `never` is compatible with `any`.
`never` **can** be assigned to `any`, but `any` **cannot** be assigned to `never` (because `never` is a type without any inhabitants).

Any value can be assigned to `any`:

```typescript
const x: any = 1;
const y: any = { what: "ever" };
```

But no value can be assigned to `never`:

```typescript
const x: never = 1;
// ~ Type 'number' is not assignable to type 'never'.
```

Not even `any` can be assigned to `never`:

```typescript
const x: any = 1;
const y: never = x;
// ~ Type 'any' is not assignable to type 'never'
```

* [TypeScript Playground: any,window=_blank](https://www.typescriptlang.org/play?jsx=0&pretty=true&preserveWatchOutput=false&inlineSourceMap=false&removeComments=true#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMCFBMmEAKJkAloggo8xwwBtTpMhHTxCERXNOJKtPEZMQOxAOYuLPG1hPF4MU2FUVABrAFtidGj9Ql9xLQAKXntVfUUIdIBKCABvUAgIaQSzREsIAF4IACJSAE8ITGwAFRaOPABlOXR9DkRGgG4yiAo5eFi8JXFEQPrqywmAX0KCidAFQlojc1RfbABhRQZLAG1Go99GgF0Vvcu8cTvxYWSydJfUS22mhAGkgp1IEDE+l8DlaFWI5ngLkoAANWsi0hoXkYAB7YWENACMO2B6mYADlKJUEaZULwIYQ2ogeqY5OCQhD6FDCHgbK4IMjudICOitFiIC1sIKCCsidoAH4Qbq9CAAckIsxC6BVEH0tAghFQRkh0OIwlqfKZyrVMgIKrSTBBzGR2OROr1qIZyIANBAAO7IfRyZAQVmEA1GdnG7m8lFS9CuvCGZAEDFgMUS-U29ArbETSAKpWmFWtbW6-WGjm0Lmm82US1FuMqoA)

## Operations on any

A value of type `any` allows any operations whatsoever on that value, while a value of type `unknown` allows no operations unless runtime checks are performed first to serve as guards.

Suppose we start with this:

```typescript
let yoda: any;
```

Because we explicitly annotated `yoda` with `any`, we disabled type-checking for it.
Not even the default type inference will come into play.
Therefore, we can read or write to `yoda` or assume it is an array, or object, or whatever, and the type-checker will remain silent about everything.

```typescript
log(yoda.name);            // ①
yoda.name = "Master Yoda"; // ②
```

1. We don’t know if `yoda` is an object and has a `name` property.
2. Same as above.
Therefore, we don’t know if we can assign a value to it.

```typescript
yoda = null;                // ①
yoda.power.toString();      // ②
log(yoda.power.toFixed(2)); // ③
```

1. Assign `null` to `yoda`.
2. Then of course it shouldn’t allow reading any properties and invoking methods on them.
3. Same as point 2.

Almost everything we did to the value `yoda` above would cause runtime errors, but we disabled all type-checking by explicitly annotating `yoda` to be of type `any`.
And the type checker rests its case and leaves us on our own.
And we we’ll get runtime errors, rollbacks, and someone will be held accountable, and will suffer the consequences of their ill type deeds and undoings.

* [TypeScript Playground: Operations on any,window=_blank](https://www.typescriptlang.org/play?#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMCFBMmEAErxCEFAEtaEYXl4Y8Z1KgDWAW2Lo7RwgHNxWgBS99qo0UIbwBKCABvUAgIaWdDI0QAG2sAXggAIgB5DgJiREDCUyDSAE8ITGwAFRKcgGU5dCMORHSAbiiICjl4BzwlcXyk1Pih9oBfUJD20AVCxAhE1HdsAGFFBmSAbXTF93SAXQg02Y28cV3xYTcybxPUZKnNEA1IAAMS1DJiV4gTCFReIYatZXqVXuIIABJeYKeCJMhmaxiIzuQh4BGkf7CABWeFUABptMQILREI0PPj-ugIKV-igCECcv9AbFEvA8JSMBBCKhjB4afNiIlEl4NMl5h8vthStNnupmLBCGUOOhUDl0IgynJMeYIOg8MQEVyAO6Ndh9QyoUVgXbeSXEcQkXqPe2O4i9I4ZACydEQDIAmp9iG0ns9tDoDUbqaaEtZEJR6TEheyqTSlRAVWqCJrmf94OhtHI0KhaG53L9TELFsb0dbQPbPYQ4Yl2q6OKga+gBqhamSy6F2ra2x2CN2cEYAB7o7wAJhCjxezAA6tYKIQAOTzOw842-QGvdudn5-Ymk8nuTnU4lNhzmdCcwFGXjaGQWvATkyIUxuCDvIPgpgwC0SBYESBwS3mV90BKPlyxrTojAReNDGQaxWRTP8vh+YhhFQaRrGNVA4TIQtiHgWhrHQfR8g9AhVXQWhKWEJAIHgsgTBw5IMXTTUcgAWiLPFXA8bRhDKd8OESIw5ASRIylIHllHyflMO+S1EVzXiQTBOsgA)

## Operations on unknown

```typescript
let yoda: unknown = { id: 1, name: "Yoda" };

// ①
log(yoda.name);
log(yoda.name.toUpperCase());
// ~ 'yoda' is of type 'unknown'.
```

1. Unlike the case with `any`, `unknown` will not allow reading and writing, or performing operations.

![Yoda object with name property on unknown type](__assets/yoda-name-unknown.png)

But it is possible to apply some type-guards (runtime checks) and thus assure safety.
Therefore, `unknown` is way safer than `any` as it will only allow operations after proper checks.

```typescript
/**
 * Checks if `obj` is an object and contains the `name` property.
 */
function hasName(obj: unknown): obj is { name: unknown } {
  if (!obj || typeof obj !== "object")
    return false;

  if (!obj.hasOwnProperty("name"))
    return false;

  return true;
}

if (hasName(yoda)) // ①
  log(yoda.name);  // ②
```

1. Apply a type-guard.
2. Inside the condition block, the type-checker knows `yoda` is an object which contains the `name` property.

Yet, the `name` property itself is of the type `unknown`, so we are not allowed to assume we can call methods on it.

![Yoda name of type unknown](__assets/yoda-name-unknown-type.png)

Again we would need type checks.

```typescript
if (hasName(yoda) && typeof yoda.name === "string")
  log(yoda.name.toUpperCase());
```

Now, because we first make sure `yoda.name` is a string before attempting to invoke a string method on it, the type-checker is satisfied with it.

* [TypeScript Playground: Unknown object with name property type-guard,window=_blank](https://www.typescriptlang.org/play?removeComments=true&jsx=0&pretty=true&preserveWatchOutput=false&inlineSourceMap=false#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMCFBMmEAKJkAloggo8xwwBtTpMhHTxCERXNOJKtPEZMQOxAOYuLPG1hPF4MU2FUVABrAFtidGj9Ql9xLQAKXntVfUUIdIBKCABvUAgIaQSzREsIAF4IACIAVUJowlQAdwcRACs8VWMATw48AFpfeASbTGwAFRG8AGU5dH0OREaAbjKICjl4WLwlcURA+urLHYBfQoKd0AVCWiNzVF9sAGFFBksAbUab18jQAuhcnr88OIgeJhMkyOkIahLPdNCANJAdNJjsY0PBfMgIKRHMJ+oNOoZCV4AAb6MjUonkCDUkhHBkcdCoUboM54WjaTp0In0fS+Qh4GyuZlDVBkYjUgA0uJciwg+lozPs7S6hGp2g64pMlnoaQ0liMMrl2C1HW6F2KarI2AAjErWXhsI0AJqy4iNCDXB7o9TMb5KdAJIbGSjU0hDRWatq23UQCnmcwQDpGYTmeHmKPoPDEAwpRlkAVrM4pJUYbwEMLoWLJXyObnEM4-U1gIHpS3EcTu1E9vsD4hHU6oZocbmfOh4O47SAAPwgAHI+6u1RrULxhqM1zadau0uAAFSnsqniCfZADaIa-S76l9BnqxkkslGawQJ7KZIakxL2Zd12U5blECGNIIFPMAskIHI8mQOgADkx3nPprSTHUCmwPotxKTM0Mw7U7WuEpdkffIAEI8IAH1ovc8B3D8ICouoGkaPoBk2ApdnKQtEHgIgIF4YhzHcB5yko9IaNJcQkNoAB5boAAUwIICD0kad1GgKXjyn4jwhIcUTxLwSTbCM4TEDsczQGuNF0W0ABJXdKM6UwKAgBgjlsewzl8uRbzke8lRMBxDCJdMug1eAAMYSBC2LZVvHUnlIK0aSFNQo5e19PTdmHX1RyOVFQCylC0LyuUCvKIq5RKqFXCnGc5wXXZlzXEd3U3N9mIg-dV0Pbpj0cjFmCoJBcTfNMM1odt1V4KMvAG0wgrvAhhVTKxC0zLoIHiJIUm0WghNMalurQ18H3IPAJSJbybObCAQgbKx2DwWINme1xtDkMS5qe0sjhQWVtwixAu3K3d0myqq+yKAAyRHGOYy7fPYjiXjWFJdMK95qv7d0JxaghZ3cBcgA)

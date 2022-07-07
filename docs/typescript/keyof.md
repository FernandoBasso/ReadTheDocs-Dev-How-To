---
title: keyof type operator | TypeScript
description: This text discusses concepts and ideas related to TypeScript's typeof type operator with practical examples explained.
---

# keyof

`keyof` is the *Index Type Query*.
From the docs:

> An indexed type query `keyof T`  yields the type of permitted property names for `T`.
> A `keyof T` type is considered a subtype of `string`.

The `keyof` operator takes an object type and produces a string or numeric literal union of its keys.

### keyof 2D example

```typescript
type D2 = {
  kind: "2D";
  x: number;
  y: number;
}

const h: keyof D2 = "‽";
```

What types and values could be assigned to `h`?

In the first place, `keyof D2` produces a string union of the keys in `D2` in this case because the type `D2` defines an object with three properties, and object properties are strings.
So, the `h` requires strings.
But not *any* string in the set of all possible strings.
It has to be one of `"kind" | "x" | "y"` because “keyof produces a string union of an object's keys.”
Therefore, those are the only thee possible values we can assign to `h`.

Intellisense works as expected:

![infer 2d intellisense](./keyof.assets/keyof-2D-types.png)

![infer 2d intellisense](./keyof.assets/keyof-2D-if.png)

- [TS Playground 2D/3D example](https://www.typescriptlang.org/play?#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMJBhtO3PgNIVR9VOIDmAS0QAbYsPFnUwKdLQB3RA6YstXHsBf-JGVd3QNkmTwBrPABPVF4IRGiOPAhUZPRid3RPAF48-ILCgvDQMwBbDgxEBKSUgG8IAFEAR3hiSwAaJoAPZNUIAF8IXnRUMogAIgABROSAWjlkdss8QhM8WmAkM0taCYBuUCPZlIARACYIHIg60AgICLNybAnz04O7iG7sQngy4QIh3u0R+fwB6EOA2OtQgpwAzFcbp9Hs9JnD3vsIJ9vhBfv9AZ8QbiwQT7gAvUH4iGgAaHI4KQi0apkc7YKKxeIXRETAA8iHQZhM63QECeiDwlksZloqxlrAIeAAfB9QAymRAyHC2TE4rCEddefzBcLRUoJVKZYyUsgFcq6SBPAAVdDRCCLUjmNYQQAYBGTvRAXBZkD7ot6uhgfSiyIBMAnEEEdNsICRdWMg3oi0dSIu9hDSeEz+lNtD61RQKQIo3Q4nCauqyG1HL13IiKpOsPOAGkYrREezdRdDm34V3oj3rn3OXC6W3HRtEABhOgbREAbU+jV6eFUPJabUsPIn7a6EyjEwgAB9Jt0z5eJtEJorFR115vt7v2jyLiPaMfTxerzekz3o+z4viWO6tB+h7wr+TxkIBEzXv+d4IWSD5PmBW6IBBe6fnC36weQCFIbe97IWhIGgAAuocQA).

### keyof 3D example index signature

Suppose this type is in context:

```typescript
type D3 = {
  kind: "3D"; 
  x: number;
  y: number;
  z: number;
};
```

Consider:

```typescript
declare const shape3d: D3;

let k1 = "x";

//
//    k1 indexing errors out 😭
//    -------------------------
//                \
//                 \
//                  \
const val1 = shape3d[k1];
//
// ERROR:
//
// Element implicitly has an 'any' type because expression of type
// 'string' can't be used to index type 'D3'.
//
// No index signature with a parameter of type 'string' was
// found on type 'D3'.
//
```

`k1` is assigned `"x"`, but it could have been changed since its assignment and
its use.
After all, it is a string declared with `let` (`var` would have produced the same outcome), not `const`.

`k1` is of type `string`, not the union of literal strings `"kind" | "x" | "y" | "z"`, and TypeScript knows those are the only four possible keys in `shape3d`.

It works though if we use *const context* when declaring the variable:

```typescript
declare const shape3d: D3;

var k2 = "x" as const;
let k3 = "x" as const;
const k4 = "x";

const val2 = shape3d[k2];
const val3 = shape3d[k3];
const val4 = shape3d[k4];
```

All of the above work as expected, correctly indexing the property `x` of the object.

- [TS Playground with D3 const context example](https://www.typescriptlang.org/play?#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMJBhtO3EAHd9kmWl2JUh2UyYQA1ngCeqXhER2OeCKjfpip9FYC8gUHBIcGWoC5uEAAiAMwQ-hAA3qAQNgCW5NgARLHR2QDcEKkQAB7YhPAAtsIEBSV2FdW16PVpAF5NNXWgAL71oKBScgA2xOjuCoS0iBC0yMRusWTYcQMjeLPWAIwJENmlhVYQAHoA-IOg0uM2AEx7B9kQdBBTM-WQpxegG1vxiY9nrRXop3sdzoM3lsACwPQ4fZhpL6Q0Gza4jXaJeaLPDLADaOwAugikcjQFCIOj7liFksyATbsTjmkIeTUZTiCN-nNabj6dZYkzPizvhT0bCaTj8dZoULESLQEA)

## References

- [keyof type operator on TypeScript handbook](https://www.typescriptlang.org/docs/handbook/2/keyof-types.html).


## References

- [keyof TypeScript 2.1 Release Notes](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-1.html)



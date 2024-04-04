---
title: Type Aliases and Interfaces | TypeScript
description: Notes, tips and examples on the similarities and differences between types aliases and interfaces and when to use them.
---

# Type Aliases and Interfaces

## Introduction

In the beginning, types aliases were quite different from interfaces.

Over the years, forum discussions and posts would happen from time to time about the differences, pros and cons of each one.
As new versions of TypeScript would be released, those posts and pieces of advice would become outdated.

As of TypeScript 4 (maybe earlier), there is not much difference between interfaces and type aliases.
What you can do with one, you can do with the other (albeit with different approaches).

The main difference as of TypeScript 5.x is that interfaces allow for _declaration merging_.

## Example interface vs type alias

```typescript
interface IPerson {
  id: number;
  name: string;
};

//                 ①
interface IStudent extends IPerson {
  email: string;
}

const student1: IStudent = {
  id: 1,
  name: "Aayla Secura",
  email: "aayla@jedischool.dev",
};

type TPerson = {
  id: number;
  name: string;
};

//                     ②
type TStudent = TPerson & { email: string };

const student2: TStudent = {
  id: 2,
  name: "Ahsoka Tano",
  email: "ahsoka@jedischool.dev",
};
```

1. Using `extends` to create another interface from a base interface.
2. Using the intersection operator `&` to compose together the two types.

<dl><dt><strong>📌 NOTE</strong></dt><dd>

The intersection behaves more like an union operation this example.
Read more on [Intersection Types](/typescript/intersection-types.html) and [Union Types](/typescript/union-types.html).
</dd></dl>

* [TypeScript Playground: Type Alias and Interface example,window=_blank](https://www.typescriptlang.org/play?removeComments=true&jsx=0&module=1&pretty=true&preserveWatchOutput=false&inlineSourceMap=false#code/PTBQIAkIgIIQQVwC4AsD2AnAXBAYgU3QDsBDQgE1QgCFiBnW1cYaCZRRAB1sxADMCS5VACM6DAHRk8AN2ABjVIUTE5iMCFCgAFL3iFVAS0UQtASggBvUBAjTi6CIgOIANnggBeCACIAKgE8Od1gXAzo8WghSMggASSUCXhUIiExsAKCAZTl0Aw5EbwBuawgKOXgAWzwlcSdXdy86t2KAXzNTYqYmOMJHZANIvAAPYgqONwgAd3ccvGJEdxR3WlHFwPd4WgNCAHMIYVQUbtIIbYX0JLl3aKjHdajQunEIX2R3apj0CPgXRG6BvrLVbiLqgM6JZJxAAKBAYvSsNgMZGwhEqwgIxRsJCq2FoiFyu1anXBF0hsUyiHgUiUEGGC3IkViMPQcMsJTwFWIBhcuPx2x2rU0CkIeIgeKp1UQAEZsOTKdTEJ42YjkRApQAaErYvDYbywYj+FzECCZPDldDEbyamwcrk8nzEA1GgACACs8GQBnI0KgXJIZFbQC1Ooh7r5mayvAjTqrURV0ehMRBtbyCQKg50NJB4ni5jFULwIAADOkfWhFqYbWiLN6nBIss1OYyoIIWxAYCAAMhBGlDQReFIlNK84dhxk7llpnO5qf5EGDQsUovFCoATOlBwqldGkdhV9bk6tdbBkAwANbG3ykVCBm3T+3eYin1AXt0er0+v1SaSBhegIA).

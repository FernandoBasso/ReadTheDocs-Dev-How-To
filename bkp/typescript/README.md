# TypeScript in 50 Lessons


## Tips For Learning TypeScript

**TIP 1**: Always try to figure out the types without hovering or inspecting. Learn to quiz yourself about types all the time. Only then proceed to tip 2.

For example, try (**without** hovering the types) to say the types of, `T1`, `T2`, `T3` and `T4`:

```ts
type UndefinedAsNull<T> =
  T extends undefined ? null : T;

type T1 = UndefinedAsNull<undefined>;

type T2 = UndefinedAsNull<null>;

type T3 = UndefinedAsNull<string>;

type T4 = UndefinedAsNull<null[]>;
```

**TIP 2**: Always (**but only after applying the first tip**) hover over the types or try the intellisense to get a better understanding of the types!

**TIP 3**: Always try to say or think about the name of the concept being applied at each situation. For example, “We are using an indexed access type here, and here we make use of mapped types.”

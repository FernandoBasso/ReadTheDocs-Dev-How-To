---
description: These pages describe things that I consider most important, hard or tricky in TypeScript and type systems in general.
---

# TypeScript

Why do we need types in JavaScript anyway?

First of all, JavaScript already has types. Since its inception in Netscape.
Just that it is a dynamically-, weakly-typed (not statically- and strongly-typed).
What it doesn't have is type annotations.

The fact that it has types and people ignore them is the single most frequent cause of bugs in JavaScript programs and applications.

Types help us make sure we use our code in the way it is intended to be, define a contract, and also immensely help us to convey intent, which means they also serve as documentation, making the code more self-documenting.

## Tips For Learning TypeScript

**TIP 1**: Always try to figure out the types without hovering or inspecting.
At all times, quiz yourself about types, concepts and ideas applied; about every minutiae you can think of.
Only then proceed to tip 2.

**TIP 2**: After following tip 1, always inspect the types or try the intellisense to get a better understanding of the them!
If you got something wrong while applying tip 1 in comparison to the results you get while inspecting the types, ask yourself what lead you astray, what misunderstanding you had, or what pieces of knowledge you still didn't have.

**TIP 3**: Always try to say aloud (or at least think about) the name of the concept being applied at each situation.
For example, “We are using an indexed access type here, and here we make use of mapped types.”

## TypeScript Two Responsibilities

TypeScript types are erased during transpilation (generation of vanilla, runnable JavaScript code).

`tsc` (TypeScript compiler) does two things:

- Type-checks the code.
- Transpiles the code.

Those two things are done in separate steps and are not related.
Code could be not type-checking correctly, but `tsc` will still emit JS.

People sometimes incorrectly say “My TypeScript code doesn’t compile.”, when they should really be saying “My TypeScript code does not type-check.”
TypeScript code with type errors is still transpiled unless the [noEmitOnError](https://www.typescriptlang.org/tsconfig#noEmitOnError) flag is `true`.

## Other Resources

- [TypeScript Website with docs and the handbook](https://www.typescriptlang.org/).
- [TypeScript Wiki](https://github.com/microsoft/TypeScript/wiki).
- [Stefan Baumgartner blog on TypeScript (and other topics)](https://fettblog.eu/).

```{toctree}
---
maxdepth: 6
caption: TypeScript
---

tsconfig-examples.md
structural-typing.md
any-and-unknown-top-types.md
type-aliases-and-interfaces.md
object-types.md
union-types.md
intersection-types.md
keyof.md
mapped-types.md
functions.md
infer.md
react-component-related-types.md
```

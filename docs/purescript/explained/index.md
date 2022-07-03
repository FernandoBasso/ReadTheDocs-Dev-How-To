---
title: Functions Explained | PureScript
description: Let's scrutinize type signature, implementation, evaluation and use of several functions with practical examples.
---

# PureScript Explained

In this section we'll study and understand type signature, functions, types, data constructors, etc.
We'll also study the possible implementations (many of them simplified), together with their usage and evaluation steps.

For brevity, we'll colloquially say things like “this function takes two parameters”, but that will always mean (unless explicitly noted) that it is a function that takes one parameter, and return a function that takes another parameter, that then returns the final result.

Also, sometimes we will use a short name like `f`, `g`, `fn`, etc. instead of longer names like `const`,  `append`, `identity` or `compare`.
The goal is to make evaluation steps shorter in the text.

```{toctree}
---
hidden: true
maxdepth: 6
caption: PureScript
---

const.md
apply.md
apply-flipped.md

data-list-cons.md
```

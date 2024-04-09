---
title: Guards and Early Returns | Programming
description: Notes, tips and examples of using the concept or early returns to avoid nesting the main logic of your functions and methods.
---

# Unnest Logic From Inside Conditions

We can combine _code guards_ with _early returns_ to avoid nesting logic inside conditions, making use of the so called *early returns*.

For example, instead of this:

```javascript
function f(x) {
  if (x > 0) {
    // Logic NESTED inside if.
  }
}
```

We can do this:

```javascript
function f(x) {
  if (x < 0) return 'Oops...';

  // Main logic NOT nested inside if.
}
```

By using this second approach, we reduce the need to write logic too deeply nested inside of conditionals.

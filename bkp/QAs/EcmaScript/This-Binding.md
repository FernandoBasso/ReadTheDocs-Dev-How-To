# 1. This Binding

- [1. This Binding](#1-this-binding)
  - [1.1. bare invocation](#11-bare-invocation)
  - [1.2. bind](#12-bind)

**NOTE**: Run these in a browser Node wraps the running code in some sort of function which end up modifying the way globals and other stuff behave.

## 1.1. bare invocation

```
function f() {
  log(this.x);
}
var x = 1;
f();
```
What is the output with and without strict mode enabled? Exaplain.

<details>
<summary>» Answer «</summary>

The example does a _bare invocation_ of `f`, and therefore,   in strict mode `this` inside `f` is `undefined`.


- https://developer.mozilla.org/en-US/docs/Glossary/Sloppy_mode
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode

</details>

## 1.2. bind

```
function getX() { return this.x; };

var x1 = { x: 1 }, x2 = { x: 2 };

log(
  getX(),                       // <1>
  getX.bind(x1)(),              // <2>
  getX.bind(x2)(),              // <3>
  getX.bind(x1).bind(x2)(),     // <4>
);
```

Assuming sloppy mode, what do the four lines print? Explain.

<details>
<summary>» Answer «</summary>

Line <1> prints `undefined`.

Line <2> and <3> print 1 and 2 because `getX` was explicitly bound to `x1` and `x2` respectively.

Line <4> prints 1 because even though we invoked `bind(x2)` _after_ `bind(x1)`, once bound, a function cannot be rebound. `getX` is still bound to `x1`.

</details>

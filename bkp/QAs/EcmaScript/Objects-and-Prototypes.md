# 1. QA Objects and Prototypes

Assume `l` is `var l = console.log.bind(console)`.

- [1. QA Objects and Prototypes](#1-qa-objects-and-prototypes)
  - [1.1. reference](#11-reference)
  - [1.2. Object.create() and properties](#12-objectcreate-and-properties)
  - [1.3. prototype chain](#13-prototype-chain)

## 1.1. reference

```
var o1 = {x: 1};
var o2 = o1;
```

What is `o2.x`? 1 or `undefined`? Explain.

<details>
<summary>» Answer «</summary>

It is 1. Both `o1` and `o2` point to the same object in memory.

</details>

## 1.2. Object.create() and properties

```
var o1 = {x: 1};
var o2 = Object.create(o1);
```

What is `o2.x`? Explain.

<details>
<summary>» Answer «</summary>

`o2.x` is 1 because although `o2` does not have an “own” property `x`, it fetches `x` from the `o1` up in the prototype chain.

</details>

## 1.3. prototype chain

```
var o1 = {
  x: 1,
  inc: function f() {
    return this.x || 10 + 1;
  }
};

var o2 = Object.create(o1);

l(o2.inc());
```

What is printed? Explain.
<details>

<summary>» Answer «</summary>

1 is printed because `o2` reaches to `o1`'s `x` and `inc` properties. Since `x` is 1 (truthy), 1 is returned and `10 + 1` are not executed.

</details>


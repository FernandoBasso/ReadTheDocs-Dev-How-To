# Callback

## Example 1

```js
function add(x, y) {
  return x + y;
}

var a = add(1, 2);
// 3

var b = add;
// copy of add
// b is the function add

// we can call b as a function
var c = b(1, 2);
//  3
```

## Example 2

```js
function f(n) {
  return n;
}

var a = f(1);
// 1

var b = f;
// b is a copy of f

var c = b(10);
// 10
```

## Example 3

```js
function f() {}

var a = f();
// undefined

var g = f;
// copy (reference to) of f

var p = g();
// undefined
```

## Example 4

```
function f(g) {
  return g();
}
```


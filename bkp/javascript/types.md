# JavaScript Types

[TOC]

**NOTE**: Intentionally not using colors on code blocks so that we really focus on the code and carefully read every character. I recommend doing that with your editor too when you are studying. In vim, `:set filetype=text`. Or perhaps using a vim modeline on your study files:

```
/* vim: set tw=68 ft=text ai: */
```



## Variable declaration, undefined, null

Consider this:

```
var foo;
var bar = undefined;

l(foo);
// → undefined

l(bar);
// → undefined

l(baz);
// → ReferenceError: baz is not defined
```

**TIP**: To be “defined” means  ”to be declared”. `undefined` does not mean the variable is not declared/defined; it means its **value** is undefined. The error ”baz is not defined” means it is not declared. A variable can be declared (defined) but have a value of `undefined`.

This is OK. It just won't enter the `if` block:

```
var foo;

if (foo) {
  l('Tomb Raider 1996!');
}
```

But this, again, throws a `ReferenceError`:

```
var foo;

if (qux) {
  l('Tomb Raider 1996!');
}
// → ReferenceError: baz is not defined
```



typeof vs obj.constructor
-------------------------

Generates a fatal error if `thing` has not been declared or it is somehow null or undefined.

`````
thing.constructor === Function
`````

This is safer. It just returns the string “undefined” if ‘thing’ does not exist, but not a fatal error.

```
typeof thing === 'function'
```

But we have the problem that `null` and `[]` return “object”. Therefore, we may use something like this instead:

```
function isFn(thing) {
  return thing.constructor === Function;
}
```


These are OK:

```
isFn(function () {});
isFn('');
```


But what if the parameter is `null` or `undefined`? It produces “Cannot read property 'constructor' of undefined”.

```
isFn();
// → Cannot read property 'constructor' of undefined

isFn(undefined);
// → Cannot read property 'constructor' of undefined

isFn(null);
// → Cannot read property 'constructor' of null
```

Then, we have to do some checks before attempting to use `thing.constructor`.





## References

- [Check if a function is a generator](https://stackoverflow.com/questions/16754956/check-if-function-is-a-generator)


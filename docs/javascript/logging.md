---
title: Logging | JavaScript
description: Notes, tips and examples on logging (log, info, error, etc.) in JavaScript.
---

# Logging

## Free static function

A free static function is one that can be provided as a call back and it will be run fine:

**Firefox v122 console session**

```javascript
function run(cb) {
  return cb();
}

run(Math.random);
//=> 0.20621770135266349

run(crypto.randomUUID);
// ~ Uncaught TypeError: 'randomUUID' called on an object that
// ~ does not implement interface Crypto.
```

Once, `log` was not a free static method of `console`, which means this didn’t work:

```javascript
var log = console.log;

log('hello');
// ~ Illegal invocation (back in the day)
```

But we could use something like `Function.prototype.bind`:

```javascript
var log = console.log.bind(console);

log('hello');
//=> hello
```

Nowadays we can simply do `const log = console.log` and it works on most, if not all JavaScript engines.
It works with `console.info/error/table/etc.` as well.

* [Alias console.log (Stack Overflow)](https://stackoverflow.com/questions/5133649/alias-to-chrome-console-log).
* [Illegal Invocation question (Ramda repo)](https://github.com/ramda/ramda/issues/3439).

# Currying

```js
const log = console.log.bind(console);
```

Takes a regex and returns a function that takes a string and returns a boolean if the string matches the regex.

```js
const makeMatcher = regex => str => regex.test(str);
```

// A function that produces `true` if its string argument matches the regex `/foo/` (case insensitive).
const matchFoo = makeMatcher(/foo/i);

```js
log(matchFoo('may the force'));
// → false
log(matchFoo('Three foos and two little bar'));
// → true
```

Because we used `regex.test(str)` when defining `makeMatcher`, `matchFoo` produces a boolean. Had we instead preferred `str.match(regex)` when defining `makeMatcher`, our `matchFoo` function would return either `null` for no matches, or an array with matching information if there is a match.
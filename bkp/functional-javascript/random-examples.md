# Random Functional Programming Examples

- [min50](#min50)

## min50

```js
const { __, ifElse, gt, dec } = require('ramda');

/**
 * Produces the number decremented by one or 50.
 * 
 * If the input number is greater than 50, produce that number decremented
 * by one. Otherwise, produce 50.
 * 
 * min50 :: Number -> Number
 */
const min50 = num => ifElse(
  gt(__, 50),
  dec,
  () => 50,
)(num);

log(min50(50)); // → 50
log(min50(51)); // → 50
log(min50(78)); // → 77
log(min50(49)); // → 50
```

Instead of using the _constant function_ `() => 50`, we could use `always` instead:

```js
 /**
 * min50 :: Number -> Number
 */
const min50 = num => ifElse(
  gt(__, 50),
  dec,
  always(50),
)(num);
```


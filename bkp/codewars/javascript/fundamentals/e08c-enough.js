/*

https://www.codewars.com/kata/delete-occurrences-of-an-element-if-it-occurs-more-than-n-times/train/javascript

*/

const expect = require('expect');
const l = console.log.bind(console);

// deleteNth :: List-of-number -> list-of-number
// Produce array with at most N items of each value.
function deleteNth(xs, max) {
  const history = {};
  return xs.filter((x) => {
    history[x] = ~~history[x] + 1; // <1>
    return history[x] <= max; // <2>
  });
}

expect(
  deleteNth([1, 1, 1, 1], 2)
).toEqual([1, 1]);

expect(
  deleteNth([20, 37, 20, 21], 1)
).toEqual([20, 37, 21]);

expect(
  deleteNth([1, 1, 3, 3, 7, 2, 2, 2, 2], 3)
).toEqual([1, 1, 3, 3, 7, 2, 2, 2]);

expect(
  deleteNth([3, 2, 3, 3, 3, 2, 3, 2, 3, 2, 3], 2)
).toEqual([3, 2, 3, 2])

l('\nAll tests passed!\n');

/*

<1>: Take a look:

    var x;
    // → undefined
    ~x
    // → -1
    ~~x
    // → 0
    x = ~~x + 1
    // → 1
    x = ~~x + 1
    // → 2
    x = ~~x + 1
    // → 3


    const hist = {}
    hist[0] is undefined
    ~undefined is -1
    ~~undefined is 0, which means
    hist[0] = ~~hist[0] + 1 assigns 1 (0 + 1) to hist[0]
    now hist[0] is 1, therefore,
    hist[0] = ~~hist[0] + 1 assigns 2 (1 + 1) to hist[0], and
    hist[0] = ~~hist[0] + 1 assigns 3 (2 + 1) to hist[0], etc.

The hist[n] = ~~hist[n] + 1 bitwise trickery is used to assign 1 to hist[n]
when it is undefined, and also to add 1 more to hist[n] when it already
contains a numeric value. The first pass is more or less an alternative to this:

    var h = {};
    h[0] = h[0] || 1;

And in subsequent invocations, similar to this:

    h[0] = h[0] ? h[0] + 1 : 1;

Of course, remember that 0 is falsy in a boolean expression, which means guard and ternary the way we did above would produce incorrect results. We would need to explicitly check for undefined:

    h[0] !== undefined ? h[0] + 1 : 1;

The bitwise trickery does not have such problem for this specific case.



<2>: Tthe boolean return value used by `filter`.


== Links

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators


*/
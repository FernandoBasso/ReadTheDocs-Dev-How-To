//
// https://www.codewars.com/kata/youre-a-square/train/javascript
//

const l = console.log.bind(console);

/**
 * Checks whether `n` is a perfect square.
 * 
 * If `x * x = n`, then `n` is a perfect square.
 * 
 * ASSUME: `n` is an integral number.
 * 
 * @param {number} n
 * @return {boolean}
 */
function isSquare(n) {
  if (n === 0) return true;

  // <1>
  for (let i = 0; i * i <= n; ++i) {
    l('== i', i);
    if (n % i === 0 && n / i === i)
      return true;
  }

  return false;
}

// function isSquare(n) {
//   const half = n / 2;
// 
//   for (let i = 0; i < half; ++i)
//     if (i * i === n) return true;
// 
//   return false;
// };


l(isSquare(-1));
// → false, Negative numbers cannot be square numbers.

l(isSquare(0));
//, true, 0 is a square number: 0 * 0.

l(isSquare(3));
// → false, 3 is not a square number.

l(isSquare(4));
// → true, 4 is a square number: 2 * 2.

l(isSquare(9));
// → true, 9 is a square number: 3 * 3.

l(isSquare(25));
// → true, 25 is a square number: 5 * 5.

l(isSquare(26));
// → false, 26 is not a square number.


/*
--------------------------------------------------------------------------------
https://www.quora.com/What-is-the-quickest-way-to-determine-if-a-number-is-a-perfect-square

Mind-blowing stuff:
https://math.stackexchange.com/questions/41337/efficient-way-to-determine-if-a-number-is-perfect-square

NOTE: The solution was intentionally done without `Math.sqrt` as a pure
exercise for a way to find a solution with a more primitive algorithm.

<1>: `i * i <= n` avoids that we keep looping past the point where `i * i` would
equal `n`, thus preveting unnecessary loops. We could also have done something
like `i <= n / 2` for that purpose

Instead of this:

    if (n % i === 0 && n / i === i)

We could also do this:

    if (n % i === 0 && i * i === n)

 
*/
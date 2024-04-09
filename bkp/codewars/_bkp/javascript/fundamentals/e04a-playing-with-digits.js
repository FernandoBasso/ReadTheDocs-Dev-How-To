//
// https://www.codewars.com/kata/playing-with-digits/train/javascript
//

const l = console.log.bind(console);

/**
 * @param {number} n
 * @param {number} p
 */
function digPow(n, p) {
  const sum = n.toString()
    .match(/\d/g) // <1>
    .map(Number) 
    .reduce((acc, num, idx) => {
      return acc += Math.pow(num, p + idx); // <2>
    }, 0);

  return sum % n === 0 ? sum / n : -1;
}

l(digPow(89, 1));
// → 1 since 8¹ + 9² = 89 = 89 * 1

l(digPow(92, 1));
// → -1 since there is no k such as 9¹ + 2² equals 92 * k

l(digPow(695, 2));
// → 2 since 6² + 9³ + 5⁴= 1390 = 695 * 2

l(digPow(46288, 3));
// → 51 since 4³ + 6⁴+ 2⁵ + 8⁶ + 8⁷ = 2360688 = 46288 * 51

/*

- Turn the number into an array of its digits.
- Sum each digit taken to the successive power of `p`.
- If `sum % n === 0`, we found the `k` we were seeking, as per
  the challenge requisites.

<1> `match` returns an array when used with the `g` flag. Useful here!

<2> `p + idx` is used for “the successive power of `p`”.

*/

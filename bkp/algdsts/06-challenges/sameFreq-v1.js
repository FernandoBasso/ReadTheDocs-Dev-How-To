//
// Write a function called sameFreq. Given two positive integers, find
// out if the two numbers have the same frequency of digits.
//
// Your solution MUST have the following complexities:
// • Time: O(N)
//
// Sample Input:
//
// sameFrequency(182, 281) // true
// sameFrequency(34, 14) // false
// sameFrequency(3589578, 5879385) // true
// sameFrequency(22, 222) // false
//

/**
 * Checks whether both arrays have the same frequency of digits.
 *
 * **TIME COMPLEXITY**: `O(n)`. Two loops one after the other (O(n)) is
 * much better than two nested loops `O(n ^ 2)`.
 *
 * **SPACE COMPLEXITY**: `O(n)`. We create an object to store the
 * frequencies that could get as large as the input.
 *
 * @param number xs
 * @param number ys
 * @return {boolean}
 */
function sameFreq(xs, ys) {
  const strXs = String(xs);
  const strYs = String(ys);

  if (strXs.length !== strYs.length) return false;

  const freqXs = {};

  for (let x of strXs) {
    freqXs[x] = (freqXs[x] || 0) + 1;
  }

  for (let y of strYs) {
    if (!freqXs[y]) return false; // <1>
    freqXs[y] -= 1;
  }

  return true;
}

//
// Since the inputs are two numbers (and not two arrays of numbers), we
// have to first convert both to strings. Then, make a frequency counter
// of each digit of the first string using a loop, and then finally loop
// a second time to check the frequencies of the digits in the first
// string against the frequency counter generated from the string.
//
// NOTE: It doesn't really matter which string you make a frequency
// counter from.
//
// === <1> ===
//
// We could replace
//
//   if (!freqXs[y])
//
// with
//
//   if (freqXs[y] === undefined || freqXs[y] === 0)
//
// This second option is more verbose and self-documenting. The first
// one relies on the fact that in case freqXs[y] is 0, it evalutes to
// false in the condition. The code is shorter but less explicit. I
// think both are fine.
//

export { sameFreq };

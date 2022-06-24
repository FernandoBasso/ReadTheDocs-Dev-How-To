//
// Write a function called same, which accepts two arrays. It should
// return true if every value in the array has it's corresponding value
// squared in the second array. The frequency of values must be the
// same. The order doesn't matter.
//

/**
 * Returns true if each element in `xs` has a matching squared element in
 * `ys`. Order doesn't matter. Frequency matters.
 *
 * **TIME COMPLEXITY**: `O(n²)`. We have only one visible loop, but
 * `indexOf` also loops until it finds (or reaches the end of the
 * array). It is actually nested loops.
 *
 * **SPACE COMPLEXITY**: `O(1)`. We just store the found index each
 * time. When we `ys.splice()`, we are not creating further arrays. Just
 * reusing the same `ys` from the input, but making it smaller each
 * time. We can sure assume it is constant space.
 *
 * This solution involves looping while we have matching elements in
 * `xs`. If we reach the end of the loop without getting and `indexOf`
 * of -1 it means every element in `xs` has a matching squared element
 * in `ys`.
 *
 * In <1>, we remove that element from `ys` so that one occurrence
 * doesn't ever match again. Important because we want to match the
 * frequencies of squares of values in `xs` in `ys`.
 *
 * @param {number[]} xs
 * @param {number[]} ys
 * @return {boolean}
 */
function same(xs, ys) {
  if (xs.length !== ys.length) return false;

  for (let i = 0; i < xs.length; ++i) {
    let foundIndex = ys.indexOf(xs[i] ** 2);
    if (foundIndex === -1) return false;
    ys.splice(foundIndex, 1); /* <1> */
  }

  return true;
}

export { same };

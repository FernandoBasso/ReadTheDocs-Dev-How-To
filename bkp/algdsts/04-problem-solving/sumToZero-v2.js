/**
 * Return pair of ints that sum to 0.
 *
 * ASSUME: The array is sorted. This function does not attempt to check
 * if the array is sorted neither does this function sort it.
 *
 * This solution uses the multiple pointers pattern.
 *
 * TIME COMPLEXITY: O(n) because we iterate over the array a single time
 * (and not always the entire array).
 *
 * SPACE COMPLEXITY: O(1) because we don't use extra space besides the
 * i, j and sum variables.
 *
 * @param {number} ints An array of SORTED integers.
 * @return {array[]|undefined}
 */
function sumToZero(ints) {
  if (!Array.isArray(ints) || ints.length < 2) return undefined;

  let l = 0;
  let r = ints.length - 1;
  let sum;

  // Could be ‘l < r’ as well.
  // <1>
  while (l !== r) {
    sum = ints[l] + ints[r];

    if (sum === 0) {
      return [ints[l], ints[r]];
    }

    if (sum < 0) ++l;
    else --r;
  }
}

export  { sumToZero };

/*
 * === <1> ===
 * Handles the case where the logic inadvertently compares the sum of
 * 0 + 0 === 0. It could happen if no other pair matches a sum of 0 but
 * we have a 0 in the array, and the indexes in loop are the same for
 * the left and right pointers.
 *
 * So, if for example we have the list [-1, 0, 2], and l = 1 and r = 1,
 * then ints[l] + ints[r] === 0, and we would INCORRECTLY return a pair
 * of [0, 0].
 *
 * We may have a pair [0, 0] if our input array actually contains two
 * zeroes. For example, [-1, 0, 0, 2, 3], then we would find a pair
 * [0, 0] which when summed is zero, and it would be correct.
 */

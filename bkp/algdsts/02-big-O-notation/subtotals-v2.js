import {
  addIndex,
  reduce,
  sum,
  take,
} from 'ramda';

const sumFrom0To = (idx, xs) => sum(take(idx, xs));

/**
 * Returns the subtotals of the sum of the array.
 *
 * **TIME COMPLEXITY**: `O(n²)`. Quadratic because a reduce is
 * performed, and then inside it, a sum of all elements up to a certain
 * index is performed. More ore less the same idea of a loop inside a
 * loop.
 *
 * **SPACE COMPLEXITY**: `O(n)`. The result accumulator increases as the
 * input array gets larger.
 *
 * @param {Array<number>} xs
 * @return {Array<number>}
 */
function subtotals(xs) {
  return addIndex(reduce)((acc, _, i) => {
    return [...acc, sumFrom0To(i + 1, xs)];
  }, [], xs);
}

export { subtotals };

/**
 * Checks if there are any duplicate values among the arguments.
 *
 * **TIME COMPLEXITY**: `O(1)` if we consider our own code does not does
 * any explicit iteration. Still, creating the set from the input
 * probably causes the engine o do some sort of iteration internally.
 *
 * **SPACE COMPLEXITY**: `O(n)` because we create a set potentially of
 * the size of the input.
 *
 * This solution uses a Set. It simply creates a set from the arguments
 * passed, which automatically discards any duplicates. If the created
 * set has a different ‘size’ than the input args ‘length’, then it must
 * be the case that there were duplicate elements that were discarded,
 * and we know the input contains duplicate elements.
 *
 * @param {...(number|string|nul|undefined)}
 * @return {boolean}
 */
function areThereDups(...args) {
  if (args.length === 0) return false;
  return new Set(args).size !== args.length;
}

export { areThereDups };

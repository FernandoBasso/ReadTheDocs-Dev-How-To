//
// Write a function that counts the number of each alphanumeric
// character in the given string.
//
// We want to return an object containing only the count of the
// alphanumeric characters that do appear on the input string.
//
// The input string may contain uppercase characters but we count
// uppercase and lowercase characters as being the same. E.g. “e” and
// “E” are to be considered the same.
//

/**
 * Count the number of alphanumeric characters in the input string.
 *
 * **TIME COMPLEXITY**: `O(n)`. We iterate over each element in turn.
 *
 * **SPACE COMPLEXITY**: `O(1)`. There is only a specific subset of
 * chars we count (alphanumeric), so, the produced accumulator object
 * never gets larger than that. We can consider it constant space.
 *
 * @param {string}
 * @return {object}
 */
function countAlphaNum(str) {
  if (str.length === 0) return {};

  return [...str.replace(/[^a-zA-z0-9]/g, "")].reduce((acc, chr) => {
    const k = chr.toLowerCase(chr);

    if (acc.hasOwnProperty(k)) {
      acc[k] += 1;
      return acc;
    }

    acc[k] = 1;
    return acc;
  }, {});
}

export { countAlphaNum };

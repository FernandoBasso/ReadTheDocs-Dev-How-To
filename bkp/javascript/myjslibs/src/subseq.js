
/**
 * Returns the longest string in the array.
 *
 * @param {array} arr
 * @return {string}
 */
export function longest(arr) {
  if (!arr || arr.constructor !== Array || arr.length === 0) {
    return null;
  }

  let longestSoFar = '';

  for (const currentString of arr) {
    if (currentString.length >= longestSoFar.length) {
      longestSoFar = currentString;
    }
  }

  return longestSoFar;
}


/**
 * Makes a map of each char's position in the string.
 *
 * @example
 * // Returns `{b: [0, 2], o: [1]}`.
 * strToMap('bob');
 *
 * @param {string} str
 * @return {object}
 */
export function strToMap(str) {
  const map = {};

  for (let i = 0; i < str.length; ++i) {
    let chr = str[i];

    if (map[chr] === undefined) {
      map[chr] = [i];
    }
    else {
      map[chr].push(i);
    }
  }

  return map;
}


/**
 * Checks whether all letters in str exist as keys of map.
 *
 * @param {string} str
 * @param {object} map
 *
 * @example
 * // Neither ‘j’ or ’s’ exist in the object.
 * exists('js', { c: [0], s: [1, 2]});
 * // → false
 *
 * @return {boolean}
 */
export function exists(str, map) {
  for (let chr of str) {
    if (map[chr] === undefined) return false;
  }

  return true;
}


/**
 * Calculates the next index...
 *
 * @TODO: write proper docs for this function.
 *
 * @param {number} minIndex
 * @param {number[]} indices
 *
 * @example
 * // Zero appears on the first index, so, return 1,
 * // which is the next index where the next char
 * // should appear.
 * nextIndex(0, [0, 3, 5]);
 * // → 1
 *
 * @return {number|false}
 */
export function nextIndex(indices, minIndex) {
  for (let idx of indices) {
    if (idx >= minIndex) {
      return idx + 1;
    }
  }

  return false;
}

/**
 * Checks whether a string is a subsequence of another string.
 *
 * @param {string} str
 * @param {object} map
 *
 * @example
 * // 'bus' is a subsequence of 'abacus' because the 'b', 'u',
 * // and 's' all appear in the same order in 'abacus'.
 * isSubsequence('bus', 'abacus')
 * // → true
 *
 * @example
 * // 'abe' is NOT a subsequence of 'bale', because the 'a',
 * // 'b', and 'e' are not in the same order.
 * isSubsequence('abe', 'bale')
 * // → false
 *
 * @return {boolean}
 */
export function isSubsequence(str, map) {
  let minIndex = 0;

  // Loop throug str and see if each char of the string
  // at least exists in the map.
  for (let chr of str) {
    if(!exists(chr, map)) return false;

    // If chr exists in map, check wheter it returns
    // a next index. If so, we keep looping.
    minIndex = nextIndex(map[chr], minIndex);

    // If there is no next index, it means chr did not
    // appear in a valid position and therefore we can
    // stop and return false. str is NOT a subsequence
    // of the string used to build ‘map’.
    if (minIndex === false) return false;
  }

  return true;
}


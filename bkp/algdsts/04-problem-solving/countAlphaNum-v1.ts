/**
 * Count the number of alphanumeric characters in the input string.
 *
 * **TIME COMPLEXITY**: `O(n)`. We iterate over each element in turn.
 *
 * **SPACE COMPLEXITY**: `O(1)`. There is only a specific subset of
 * chars we count (alphanumeric), so, the produced accumulator object
 * never gets larger than that. We can consider it constant space.
 *
 * @param str The string to count the frequencies from.
 * @returns A map-like object with the char frequencies.
 */
function countAlphaNum(str: string): Record<string, number> {
  if (str.length === 0) return {};

  return [...str.replace(/[^a-zA-z0-9]/g, "")].reduce((acc, chr) => {
    const key = chr.toLowerCase();
    acc[key] = (key in acc) ? acc[key] + 1 : 1;
    return acc;
  }, {} as Record<string, number>);
}

export { countAlphaNum };

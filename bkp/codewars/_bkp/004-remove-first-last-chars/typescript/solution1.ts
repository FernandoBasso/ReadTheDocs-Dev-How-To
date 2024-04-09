const l = console.log.bind(console);

/**
 * Removes first and last chars of the input string.
 *
 * ASSUME: Input string is never less than 3 chars long.
 */
export function chopSides(s: string): string {
  //
  // `slice` goes from `iniIndex` (inclusive) to `endIndex` (exclusive).
  // -1 means the index of the last char. Since `endIndex` is exclusive,
  // that index is not returned in the resulting string, effectively
  // excluding the last char.
  //
  // Also, `Array.prototype.slice` is pure; it _does not_ modify the
  // original array. It returns a copy with specified indexes.
  //
  return s.slice(1, -1);
};

l(chopSides('eloquent'));
// ⇒ loquen

l(chopSides('country'));
// ⇒ ountr

l(chopSides('person'));
// ⇒ erso

l(chopSides('place'));
// ⇒ lac


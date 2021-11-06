/**
 * Retruns a lexicographically sorted list of strings in `xs` which
 * are substrings of the strings in `ys`.
 *
 * ASSUME: parameters are valid lists of strings.
 */
function inArray(xs: string[], ys: string[]): string[] {
  return xs.filter(function f (x: string): string | undefined {
    return ys.find(function g (y: string): boolean {
      return y.includes(x);
    });
  }).sort();
}

export { inArray };


const l = console.log.bind(console);

export function getMiddle(s: string): string {
  //
  // The base case.
  //
  if (s.length <= 2) {
    return s;
  }

  //
  // Recurse until we reach the base case.
  //
  return getMiddle(s.slice(1, -1));
}

l(getMiddle('OS'));
// ⇒ OS

l(getMiddle('Linux'));
// ⇒ n

l(getMiddle('TypeScript'));
// ⇒ Sc

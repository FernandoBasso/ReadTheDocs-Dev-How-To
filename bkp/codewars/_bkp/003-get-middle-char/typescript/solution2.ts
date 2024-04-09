
const l = console.log.bind(console);

export function getMiddle(s: string): string {
  const len: number = s.length;
  const mid: number = Math.floor(len / 2);

  //
  // Return the one or two chars. No slicing required.
  //
  if (len <= 2) {
    return s;
  }

  //
  // Odd length. Return the middle char.
  //
  if (len % 2 !== 0) {
    return s.slice(mid, mid + 1);
  }

  //
  // Even length, return the two middle chars.
  //
  return s.slice(mid, mid + 2);
}

l(getMiddle('OS'));
// ⇒ OS

l(getMiddle('Linux'));
// ⇒ n

l(getMiddle('TypeScript'));
// ⇒ Sc

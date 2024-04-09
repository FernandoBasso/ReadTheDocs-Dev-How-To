//
// https://www.codewars.com/kata/54c27a33fb7da0db0100040e/train/typescript
//

const l = console.log.bind(console);

export function isSquare(n: number): boolean {
  //
  // Use native methods to check whether the square root
  // of the number is an integer.
  //
  return Number.isInteger(Math.sqrt(n));
};

l(isSquare(11));
// ⇒ false

l(isSquare(25));
// ⇒ true

l(isSquare(4));
// ⇒ true


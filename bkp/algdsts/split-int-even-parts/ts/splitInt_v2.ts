/**
 * Divide an integer into n even parts (or as evenly as they can be).
 *
 * @param num The integer number to split.
 * @param parts The number of parts to split `num` into.
 * @returns
 */
function splitInt(num: number, parts: number): number[] {
  const rem: number = num % parts;
  const div: number = (num - rem) / parts;

  return new Array(parts).fill(div).fill(div + 1, parts - rem);
}

export { splitInt };

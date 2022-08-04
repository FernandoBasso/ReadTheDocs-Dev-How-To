/**
 * Divide an integer into n even parts (or as evenly as they can be).
 *
 * @param num The integer number to split.
 * @param parts The number of parts to split `num` into.
 * @returns
 */
function splitInt(num: number, parts: number): number[] {
  const rem = num % parts;
  const div = Math.floor(num / parts);

  const leftoverParts = (num - (parts - rem) * div) / rem;

  return new Array(parts).fill(div).fill(leftoverParts, parts - rem);
}

export { splitInt };

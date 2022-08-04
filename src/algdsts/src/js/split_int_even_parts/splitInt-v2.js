const log = console.log.bind(console);

/**
 * Divide an integer into even parts (or as evenly as they can be).
 *
 * @example
 * spitInt(10, 1);
 * // → [10]
 *
 * @example
 * split(10, 3);
 * // → [3, 3, 4]
 *
 * @example
 * split(20, 6);
 * // → [3, 3, 3, 3, 4, 4]
 *
 * @param num The integer number to split.
 * @param parts The number of parts to split `num` into.
 * @returns
 */
function splitInt(num, parts) {
  const rem = num % parts;
  const div = Math.floor(num / parts);

  const leftOverParts = (num - (parts - rem) * div) / rem;

  return new Array(parts).fill(div).fill(leftOverParts, parts - rem);
}

log(splitInt(10, 1));
log(splitInt(2, 2));

log(splitInt(10, 2));
log(splitInt(10, 3));
log(splitInt(20, 6));

log(splitInt(47, 15));

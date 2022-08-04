const log = console.log.bind(console);

/**
 * Divide an integer `num` into `parts` even parts (or as
 * even as they can be).
 *
 * ASSUME: `num` can always be evenly divided.
 *
 * This version results with the largest numbers first.
 *
 * @example
 * spitInt(10, 1);
 * // → [10]
 *
 * @example
 * split(10, 3);
 * // → [4, 3, 3]
 *
 * @example
 * split(20, 6);
 * // → [4, 4, 3, 3, 3, 3]
 *
 * @param num The integer number to split.
 * @param parts The number of parts to split `num` into.
 * @returns
 */
function splitInt(num, parts) {
  return [...Array(parts)].map((_, i) => {
    return 0 | num / parts + (i < num % parts);
  });
}

log(splitInt(10, 1));
log(splitInt(2, 2));

log(splitInt(10, 2));
log(splitInt(10, 3));
log(splitInt(20, 6));

log(splitInt(47, 15));

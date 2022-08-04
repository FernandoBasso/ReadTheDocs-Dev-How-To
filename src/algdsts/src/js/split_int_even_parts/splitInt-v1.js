const log = console.log.bind(console);

function sum(xs) {
  return xs.reduce((acc, x) => acc + x, 0);
}

// /**
//  * @param {number} num
//  * @param {number} parts
//  * @return {number[]}
//  */
// function splitInt(num, parts) {
//   // Number of last elements that will have to be
//   // changed later.
//   var rem = num % parts;
//
//   var div = Math.floor(num / parts);
//
//   var allDiv = new Array(parts - rem).fill(div);
//
//   var lastN = new Array(rem).fill((num - sum(allDiv)) / rem);
//
//   return {
//     div,
//     rem,
//     allDiv,
//     lastN,
//     solved: [...allDiv, ...lastN],
//   };
// }


/**
 * Divide an integer `num` into `parts` even parts (or as
 * even as they can be).
 *
 * ASSUME: `num` can always be evenly divided.
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
  // Number of last elements that will have to be
  // changed later.
  var rem = num % parts;

  var div = Math.floor(num / parts);

  var allDiv = new Array(parts - rem).fill(div);

  var lastN = new Array(rem).fill((num - sum(allDiv)) / rem);

  return {
    div,
    rem,
    allDiv,
    lastN,
    solved: [...allDiv, ...lastN],
  };
}

log(splitInt(10, 1));
log(splitInt(2, 2));

log(splitInt(10, 2));
log(splitInt(10, 3));
log(splitInt(20, 6));

log(splitInt(47, 15));


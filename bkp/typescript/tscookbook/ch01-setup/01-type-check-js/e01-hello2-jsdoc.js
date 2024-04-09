// @ts-check

//
// One can some values with JSDoc which causes TypeScript to follow
// the hints on the JSDoc tags to help with the types.
//

import { log } from "../lib/loggers";

/** @type {number} */
let x;

if (Math.random() < 0.5)
  x = "hello"; // <2>

/**
 * Return price with vat included.
 *
 * @param {number} price
 * @param {number} [vat = 0.2]
 * @return {number}
 */
function addVAT(price, vat = 0.2) {
  return price * (1 + vat);
}

//           <5>
addVAT(1000, "0.2");

//           <6>
addVAT(1000).toUpperCase();

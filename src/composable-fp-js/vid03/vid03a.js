import { log } from '../lib/lib.js';

/**
 * Attempts to get a color value by name.
 *
 * @param {string} name The color name, like ‘red’, ‘green’, or ‘blue’.
 * @return {undefined|string}
 */
function findColor(name) {
  return {
    red: '#ff4444',
    green: '#0fa00f',
    blue: '#3b5998'
  }[name];
}

//
// OK
//
const blue = findColor('blue').slice(1);

//
// NOK
//
// Blows up because can't slice on undefined.
//
const yellow = findColor('yellow').slice(1);
// TypeError: Cannot read property 'slice' of undefined
// 😲

log(blue, yellow);
// Doesn't even log because an exception happens before
// we reach this log.

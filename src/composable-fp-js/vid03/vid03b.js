/// <reference path="./typedefs.js" />

import { log } from '../lib/lib.js';
import { Left, Right } from './Either.js';

/**
 * Attempts to get a color value by name.
 *
 * @param {string} name The color name, like ‘red’, ‘green’, or ‘blue’.
 * @return {Either}
 */
function findColor(name) {
  const color = {
    red: '#ff4444',
    green: '#0fa00f',
    blue: '#3b5998'
  }[name];

  return color ? Right(color) : Left(undefined);
}

const red = findColor('red')
  .map(color => color.slice(1))
  .fold(_ => 'No color', hexStr => hexStr.toUpperCase());

const yellow = findColor('yellow')
  .map(color => color.slice(1))
  .fold(_ => 'No color', hexStr => hexStr.toUpperCase());

log(red, yellow);
// → FF4444
// → No color

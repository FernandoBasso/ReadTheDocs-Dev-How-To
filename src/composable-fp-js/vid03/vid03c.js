/// <reference path="./typedefs.js" />

import {
  log,
  isNil,
} from '../lib/index.js';

import { Left, Right } from './Either.js';

/**
 * Wraps the value into an `Either` type.
 *
 * @param {any} value
 * @return {Either}
 */
function fromNullable(value) {
  return isNil(value) ? Left(value) : Right(value);
}

/**
 * Attempts to get a color value by name.
 *
 * @param {string} name The color name, like ‘red’ or ‘blue’.
 * @return {Either}
 */
function findColor(name) {
  return fromNullable({
    red: '#ff4444',
    green: '#0fa00f',
    blue: '#3b5998'
  }[name]);
}

const yellow = findColor('yellow')
  .map(color => color.slice(1))
  .fold(_ => 'No color', hexStr => hexStr.toUpperCase());

const blue = findColor('red')
  .map(color => color.slice(1))
  .fold(_ => 'No color', hexStr => hexStr.toUpperCase());

log(
  yellow,
  blue,
);
// → No color
// → FF4444


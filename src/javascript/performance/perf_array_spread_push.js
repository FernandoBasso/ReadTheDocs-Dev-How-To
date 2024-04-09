//
// Make sure package.json contains "type": "module". Then,
// run the code with a command like this:
//
//   $ node --experimental-modules ./file.js
//

import { performance as perf } from 'node:perf_hooks';

const log = console.log.bind(console);

function toInt(n) {
  return n | 0;
}

////
// An array of 10000 numbers.
//
var nums = Array.apply(null, { length: 1e5 })
  .map(Function.call, Math.random);

////////////////////////////////////////////////////////////////////////
// push()
//
var pushIni = perf.now();

var numsCopy1 = nums.reduce((acc, n) => {
  acc.push(n);
  return acc;
}, []);

var pushEnd = perf.now();

log('PUSH:', toInt(pushEnd - pushIni));

////////////////////////////////////////////////////////////////////////
// spread
//
var spreadIni = perf.now();

var numsCopy2 = nums.reduce((acc, n) => {
  return [...acc, n];
}, []);

var spreadEnd = perf.now();

log('SPREAD:', toInt(spreadEnd - spreadIni));

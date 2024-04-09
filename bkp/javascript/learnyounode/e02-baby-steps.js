'use strict';

const l = console.log.bind(console);

const add = (x, y) => x + y;

const toInt = (x) => parseInt(x, 10);

// Ignore ‘node’ and the path to the script (first two args) but
// collect the remaining args so we can add them together later.
const [ , , ...nums ] = process.argv;

// ASSUME: ‘nums’ contains only numbers.
const total = nums.reduce((acc, n) => acc + toInt(n), 0);

l(total);


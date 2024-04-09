'use strict';

const l =  console.log.bind(console);

const { readFileSync } = require('fs');

const buf = readFileSync(process.argv[2]);

const str = buf.toString();

const lines = str.split('\n');

// According to the instructions, the input file does not contain
// a newline after the last line, therefore, we subtract 1.
l(lines.length - 1);


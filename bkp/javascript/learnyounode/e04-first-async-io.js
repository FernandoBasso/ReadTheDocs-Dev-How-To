'use strict';

const { readFile } = require('fs');

const l = console.log.bind(console);
const e = console.error.bind(console);

const filename = process.argv[2];

const countLines = function countLines (err, str) {
  if (err) {
    e('Error: ', err);
    return null;
  }

  // According to the instructions, the input file does not
  // contain a newline after the last line, therefore, we subtract
  // 1.
  l(str.split('\n').length - 1);
}

readFile(filename, 'utf8', countLines);


'use strict';

const { readdir } = require('fs');
const { extname } = require('path');

const dir = process.argv[2];
const ext = '.' + process.argv[3];

// ‘forEach’ callback accepts 3 params, but we only need to use
// the first one.
const print1 = (arg) => console.log(arg);

const cb = function cb (err, list) {
  if (err) return console.error(err);

  const names = list.reduce((acc, s) => {
    if (extname(s) === ext) return [...acc, s];
    return acc;
  }, []);

  names.forEach(print1);
};

readdir(dir, cb);


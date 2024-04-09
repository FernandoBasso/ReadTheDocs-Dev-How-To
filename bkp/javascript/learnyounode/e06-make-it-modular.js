'use strict';

const reader = require('./e06-my-module');

const dir = process.argv[2];
const ext = process.argv[3];

// ‘forEach’ callback accepts 3 params, but we only need to use
// the first one.
const print1 = (arg) => console.log(arg);

const print1Cb = function print1Cb (err, arr) {
  if (err) {
    return console.error('There was an error:', err);
  }

  arr.forEach(print1);
};

reader(dir, ext, function print1Cb (err, arr) {
  if (err) {
    return console.error('There was an error:', err);
  }

  arr.forEach(function (name) {
    console.log(name);
  });
});


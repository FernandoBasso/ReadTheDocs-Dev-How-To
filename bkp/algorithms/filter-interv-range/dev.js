'use strict';

const l = console.log.bind(console);

const xs = [1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15];

function filterRange(min, max, list) {
  const withinRange = (low, high) => val => val >= low && val <= high;

  return list.filter(withinRange(min, max), list);
}

const res1 = filterRange(6, 10, xs);

l(res1);

/* eslint-disable no-unused-vars */

import expect from 'expect';

import {
  l,
  testsPassed,
  deepFreeze,
} from './lib';

const addCounter = (list) => {
  // Do not use array push. Rather, return an entirely new array.
  return [...list, 0];
};

const removeCounter = (list, idx) => {
  return [
    ...list.slice(0, idx),
    ...list.slice(idx + 1),
  ];
};

const incrementCounter = (list, idx) => {
  return [
    ...list.slice(0, idx),
    list[idx] + 1,
    ...list.slice(idx + 1),
  ];
};


(function testAddCounter () {
  const listBefore = [];
  const listAfter = [0];

  deepFreeze(listBefore);

  expect(addCounter(listBefore)).toEqual(listAfter);
}());


(function testRemoveCounter () {
  const listBefore = [0, 10, 20];
  const listAfter = [0, 20];

  deepFreeze(listBefore);

  expect(
    removeCounter(listBefore, 1)
  ).toEqual(listAfter);
}());

(function testIncrementCounter () {
  const listBefore = [0, 10, 20];
  const listAfter = [0, 11, 20];

  deepFreeze(listBefore);

  expect(
    incrementCounter(listBefore, 1)
  ).toEqual(listAfter);
}());

testsPassed();

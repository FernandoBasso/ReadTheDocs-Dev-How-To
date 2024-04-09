/* eslint-disable no-unused-vars */
import {
  l,
  testsPassed,
} from './lib';

import expect from 'expect';

const counter = (state = 0, action) => {
  const { type } = action;

  switch (type) {
    case 'INCR':
      return state + 1;
    case 'DECR':
      return state - 1;
    default:
      // The default case handles non-existing or unknown action types.
      return state;
  }
};

// NOTE: Always test the base and unhappy paths first.

//
// The reducer has to handle the case of a missing state, and
// use a default one if necessary.
//
expect(counter(undefined, { type: 'INCR' })).toEqual(1);
expect(counter(undefined, { type: 'DECR' })).toEqual(-1);

//
// For inexisting or unknown action types, the state should
// remain unchanged.
//
expect(counter(1, {})).toEqual(1);
expect(counter(1, { type: 'OOPS' })).toEqual(1);

expect(counter(0, { type: 'INCR' })).toEqual(1);
expect(counter(1, { type: 'INCR' })).toEqual(2);
expect(counter(2, { type: 'DECR' })).toEqual(1);
expect(counter(1, { type: 'DECR' })).toEqual(0);

testsPassed();

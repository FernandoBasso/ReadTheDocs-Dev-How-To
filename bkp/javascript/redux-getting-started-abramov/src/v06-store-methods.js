/* eslint-disable no-unused-vars */
import {
  l,
  testsPassed,
} from './lib';

import expect from 'expect';
import { createStore } from 'redux';

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

const store = createStore(counter);

const render = () => {
  document.body.innerText = store.getState();
};

store.subscribe(render);

// Render it once, even before the first click.
document.addEventListener('load', render);

document.addEventListener('click', () => {
  store.dispatch({ type: 'INCR' });
}, false);

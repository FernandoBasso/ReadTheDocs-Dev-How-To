/* eslint-disable no-unused-vars */
import {
  l,
  testsPassed,
} from './lib';

import expect from 'expect';
// import { createStore } from 'redux';

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

const createStore = (reducer) => {
  // @TODO: How to use immutable references here‽
  let state;
  let listeners = [];

  const getState = () => state;

  const dispatch = (action) => {
    state = reducer(state, action);
    listeners.forEach(listener => listener());
  };

  const subscribe = (subscriber) => {
    listeners.push(subscriber);

    // The client code can subscribe, store this returned function,
    // and use it any time to unsubscribe.
    return function unsubscribe () {
      listeners = listeners.filter(listener => listener !== subscriber);
    }
  }

  // Populate the initial state by dispatching a dummy
  // action which will cause the store to initialize itself
  // with the default state.
  dispatch({})

  return { getState, dispatch, subscribe };
};

const store = createStore(counter);

const render = () => {
  document.body.innerText = store.getState();
};

store.subscribe(render);

// Render it once, even before the first click.
document.addEventListener('load', () => {
  // not working...
  l('doc loaded')
  render();
}, false);

document.addEventListener('click', () => {
  store.dispatch({ type: 'INCR' });
}, false);

/* eslint-disable no-unused-vars */
import {
  l,
  testsPassed,
} from './lib';

import expect from 'expect';
import React from 'react';
import ReactDOM from 'react-dom';

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

export const Counter = ({ value, onIncrement, onDecrement }) => {
  return (
    <section className='counter-wrp'>
      <div>{ value }</div>
      <button onClick={ onDecrement }>--</button>
      <button onClick={ onIncrement }>++</button>
    </section>
  );
};

// Render it once, even before the first click.
const render = () => {
  ReactDOM.render(
    <Counter
      value={ store.getState() }
      onIncrement={ () => store.dispatch({ type: 'INCR '})}
      onDecrement={ () => store.dispatch({ type: 'DECR' })}
    />,
    document.getElementById('root'),
  );
};

store.subscribe(render);
render();

//
// We don't want to hard-code the store or redux dependency _inside_ the
// component. Rather, we want to pass utility/helper functions as props so they
// can be provided for tests, or even replaced with something else should the
// need arise.
//

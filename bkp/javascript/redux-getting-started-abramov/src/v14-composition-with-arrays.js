/* eslint-disable no-unused-vars */

import expect from 'expect';
import { createStore } from 'redux';

import {
  l,
  testsPassed,
  deepFreeze,
} from './lib';

const log = console.log.bind(console);

//
// We'll extract the todo logic into a separate function.
//

/**
 * The mighty `todo' reducer to handle an individual todo.
 *
 * Returns a new todo from the action data, or toggles a todo.
 *
 * In this case, `state` is the todo itself.
 *
 * @param {object} state
 * @param {object} action
 * @return {object}
 */
const todo = (state, action) => {
  switch (action.type) {
    case 'TODO_ADD':
      return {
        id: action.id,
        text: action.text,
        compl: false,
      };
    case 'TODO_TOGGLE':
      // NOTE: in this case, state is the individual todo.
      if (state.id !== action.id) return state;

      // This is the choosen one!.
      return { ...state, compl: !state.compl };
    default:
      return state;
  }
};

/**
 * The todos reducer.
 *
 * Delegates single-todo logic to the `todo' reducer.
 *
 * @param {object} state
 * @param {object} action
 * @return {object}
 */
const todos = (state = [], action) => {
  const { type } = action;

  switch (type) {
    case 'TODO_ADD':
      return [
        ...state,
        todo(undefined, action),
      ];
    case 'TODO_TOGGLE':
      return state.map(t => todo(t, action));
    default:
      return state;
  }
};


const visibilityFilter = (state = 'SHOW_ALL', action) => {
  const { type, filter } = action;
  switch (type) {
    case 'SET_VISIBILITY_FILTER':
      return filter;
    default:
      return state;
  }
};


/**
 * The main reducer of the todo application.
 *
 * Delegates responsibility to other reducers, passing them the part of the
 * state that they manage, and combines the result into the new state tree.
 *
 * @param {object} state The state tree object.
 * @param {object} action The object describing the action.
 * @return {object}
 */
const todoApp = (state = {}, action) => {
  return {
    todos: todos(state.todos, action),
    visibilityFilter: visibilityFilter(state.visibilityFilter, action),
  };
};


const store = createStore(todoApp);
log('\ninitial state:', store.getState());

log('\nadding a todo');
store.dispatch({
  type: 'TODO_ADD',
  id: 0,
  text: 'Learn Redux',
});

log('\nading another todo');
store.dispatch({
  type: 'TODO_ADD',
  id: 1,
  text: 'Play Tomb Raider I from 1996',
});

log('\nstore state');
log(store.getState());

log('\n toggling todo with id 0');
store.dispatch({
  type: 'TODO_TOGGLE',
  id: 0,
});

log('\nstore state after toggle todo id 0');
log(store.getState());

log('\ndispatching SET_VISIBILITY_FILTER');
store.dispatch({
  type: 'SET_VISIBILITY_FILTER',
  filter: 'SHOW_ALL',
});

log('\nstore state after toggle todo id 0');
log(store.getState());

testsPassed();

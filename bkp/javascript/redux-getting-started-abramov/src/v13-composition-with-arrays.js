/* eslint-disable no-unused-vars */

import expect from 'expect';

import {
  l,
  testsPassed,
  deepFreeze,
  freeze,
} from './lib';

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


(function testAddTodo () {
  const stateBefore = [];
  const stateAfter = [
    { id: 0, text: 'Learn Redux', compl: false },
  ];

  const action = {
    type: 'TODO_ADD',
    id: 0,
    text: 'Learn Redux',
  };

  deepFreeze(stateBefore);
  deepFreeze(action);

  expect(
    todos(undefined, action)
  ).toEqual(stateAfter);
}());


(function testToggleTodo () {
  const stateBefore = [
    { id: 0, text: 'Learn Redux', compl: false },
    { id: 1, text: 'Play Tomb Raider', compl: false },
  ];

  // We want to toggle the state of the todo with id of 1.
  const action = {
    type: 'TODO_TOGGLE',
    id: 1,
  }

  // The todo with ID 1 was false, toggling it should make it true.
  const stateAfter = [
    { id: 0, text: 'Learn Redux', compl: false },
    { id: 1, text: 'Play Tomb Raider', compl: true },
  ];

  deepFreeze(stateBefore);
  deepFreeze(action);

  expect(
    todos(stateBefore, action)
  ).toEqual(stateAfter);
}());


testsPassed();

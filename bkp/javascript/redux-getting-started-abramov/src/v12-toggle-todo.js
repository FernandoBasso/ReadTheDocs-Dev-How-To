/* eslint-disable no-unused-vars */

import expect from 'expect';

import {
  l,
  testsPassed,
  deepFreeze,
  freeze,
} from './lib';

const todos = (state = [], action) => {
  const { type } = action;

  switch (type) {
    case 'TODO_ADD':
      return [
        ...state,
        {
          id: action.id,
          text: action.text,
          compl: false,
        },
      ];
    case 'TODO_TOGGLE':
      // const idx = state.findIndex(todo => todo.id === action.id);
      // return [
      //   ...state.slice(0, idx),
      //   { ...state[idx], compl: !state[idx].compl },
      //   ...state.slice(idx + 1),
      // ];

      // The above works, but we can opt for this one which uses `map'.
      return state.map(todo => {
        // These are not the droids you are looking for. Move along.
        if (todo.id !== action.id) return todo;

        // Found it! Toggle `compl'.
        return { ...todo, compl: !todo.compl };
      });
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

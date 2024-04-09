/* eslint-disable no-unused-vars */

import expect from 'expect';

import {
  l,
  testsPassed,
  deepFreeze,
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




testsPassed();

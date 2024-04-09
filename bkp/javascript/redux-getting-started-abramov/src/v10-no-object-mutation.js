/* eslint-disable no-unused-vars */

import expect from 'expect';

import {
  l,
  testsPassed,
  deepFreeze,
} from './lib';

const toggleTodo = (todo) => {
  return {
    ...todo,
    compl: !todo.compl,
  };
};


(function testToggleTodo () {
  const todoBefore = {
    id: 0,
    text: 'Learn Redux',
    compl: false,
  };

  const todoAfter = {
    id: 0,
    text: 'Learn Redux',
    compl: true,
  };

  deepFreeze(todoBefore);

  expect(
    toggleTodo(todoBefore)
  ).toEqual(todoAfter);

}());

testsPassed();

/* eslint-disable no-unused-vars */

import expect from 'expect';
import { createStore, /* combineReducers */ } from 'redux';
import React, { useRef } from 'react';
import ReactDOM from 'react-dom';

import {
  l,
  testsPassed,
  deepFreeze,
} from './lib';

const log = console.log.bind(console);

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


/**
 * Reducer to handle visibility filter.
 *
 * @param {object} state
 * @param {object} action
 * @return {string}
 */
const visibilityFilter = (state = 'SHOW_ALL', action) => {
  const { type, filter } = action;
  switch (type) {
    case 'SET_VISIBILITY_FILTER':
      return filter;
    default:
      return state;
  }
};

const combineReducers = (reducers) => {
  return (state = {}, action) => {
    return Object.keys(reducers).reduce((nextState, key) => {
      nextState[key] = reducers[key](state[key], action);
      return nextState;
    }, {});
  };
};

const todoApp = combineReducers({
  todos,
  visibilityFilter,
});


let nextTodoId = 0;

const TodoApp = (state) => {
  const inputRef = useRef(null);

  return (
    <div>
      <input ref={ inputRef } type='text' />

      <button
        onClick={() => {
          store.dispatch({
            type: 'TODO_ADD',
            id: nextTodoId++,
            text: inputRef.current.value,
          });

          // Clear the input after storing its value.
          inputRef.current.value = null;
        }}
      >
        Add Todo
      </button>

      <ul>
        {
          state.todos.map((todo) => {
            const { id, text, compl } = todo;
            return (
              <li
                key={ id }
                onClick={ () => {
                  store.dispatch({ type: 'TODO_TOGGLE', id });
                }}
                style={ { textDecoration: compl ? 'line-through' : 'none' } }
              >
                { text }
              </li>
            )
          })
        }
      </ul>
    </div>
  );
};

const store = createStore(todoApp);
log('\ninitial state:', store.getState());

const render = () => {
  ReactDOM.render(
    <TodoApp
      todos={ store.getState().todos }
    />,
    document.getElementById('root'),
  );
};

// `render()' wants to be notified/called everytime there is a change
// in the state tree.
store.subscribe(render);

// Call it once to pouplate the UI, even before any state changes.
render();

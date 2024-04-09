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

import './todoapp.css';

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


const getVisibleTodos = (todos, filter) => {
  switch (filter) {
    case 'SHOW_ALL':
      return todos;
    case 'SHOW_ACTIVE':
      return todos.filter(({ compl }) => !compl);
    case 'SHOW_COMPLETED':
      return todos.filter(({ compl }) => compl);
    default:
      return [];
  }
}

const todoApp = combineReducers({
  todos,
  visibilityFilter,
});


const FilterButton = ({ filter, currentFilter, children }) => {
  if (filter === currentFilter) {
    return (
      <span>[ { children } ]</span>
    )
  }
  return (
    <button
      className='like-a-link'
      onClick={ () => {
        store.dispatch({
          type: 'SET_VISIBILITY_FILTER',
          filter,
        });
      }}
    >
      { children }
    </button>
  )
};

let nextTodoId = 0;

const TodoApp = ({ todos, visibilityFilter }) => {
  const inputRef = useRef(null);

  const visibleTodos = getVisibleTodos(todos, visibilityFilter);
  l('visibleTodos', visibilityFilter, visibleTodos);

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
          visibleTodos.map((todo) => {
            const { id, text, compl } = todo;
            return (
              <li
                currentFilter={ visibilityFilter }
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

      <div>
        Filters: {' '}
        <FilterButton
          filter='SHOW_ALL'
          currentFilter={ visibilityFilter }
        >
          All
        </FilterButton>
        {' '}
        <FilterButton
          filter='SHOW_ACTIVE'
          currentFilter={ visibilityFilter }
        >
          Active
        </FilterButton>
        {' '}
        <FilterButton
          filter='SHOW_COMPLETED'
          currentFilter={ visibilityFilter }
        >
          Completed
        </FilterButton>
      </div>
    </div>
  );
};

const store = createStore(todoApp);
log('\ninitial state:', store.getState());

const render = () => {
  ReactDOM.render(
    <TodoApp
      { ...store.getState() }
    />,
    document.getElementById('root'),
  );
};

// `render()' wants to be notified/called everytime there is a change
// in the state tree.
store.subscribe(render);

// Call it once to pouplate the UI, even before any state changes.
render();

// Add some todos so we have something to start with.
store.dispatch({
  type: 'TODO_ADD',
  id: nextTodoId++,
  text: 'Learn Redux',
});
store.dispatch({
  type: 'TODO_ADD',
  id: nextTodoId++,
  text: 'Play Tomb Raider I, 1996',
});
store.dispatch({
  type: 'TODO_ADD',
  id: nextTodoId++,
  text: 'Study the book HtDP 2e',
});

/* eslint-disable no-unused-vars */

import expect from 'expect';
import { createStore, combineReducers } from 'redux';
import React, { useRef, useEffect } from 'react';
import ReactDOM from 'react-dom';

import {
  l,
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


const Todo = ({
  text,
  compl,
  clickHandler,
}) => {
  return (
    <li
      onClick={ clickHandler }
      style={ { textDecoration: compl ? 'line-through' : 'none' } }
    >
      { text }
    </li>
  );
};

const TodoList = ({
  todos,
  onTodoClick,
}) => {
  return (
    <ul>
      {
        todos.map((todo) => {
          return (
            <Todo
              key={ todo.id }
              { ...todo }
              clickHandler={ () => onTodoClick(todo.id) }
            />
          );
        })
      }
    </ul>
  );
};

const FilterButton = ({
  active,
  onClick,
  children,
}) => {
  if (active) {
    return (
      <span>[ { children } ]</span>
    )
  }
  return (
    <button
      className='like-a-link'
      onClick={ onClick }
    >
      { children }
    </button>
  )
};

const FilterButtonContainer = ({
  filter,
  children,
}) => {
  const state = store.getState();

  return (
    <FilterButton
      active={ filter === state.visibilityFilter }
      onClick={ () => store.dispatch({
        type: 'SET_VISIBILITY_FILTER',
        filter,
      })}
    >
      { children }
    </FilterButton>
  );
};

const AddTodo = ({
  onAddClick,
}) => {
  const inputRef = useRef(null);

  return (
    <div>
      <input ref={inputRef} type='text' />
      <button
        onClick={ () => {
          onAddClick(inputRef.current.value )
        } }
      >
        Add Todo
      </button>
    </div>
  );
};

const Footer = () => {
  return (
    <div>
      Filters: {' '}
      <FilterButtonContainer
        filter='SHOW_ALL'
      >
        All
        </FilterButtonContainer>
      {' '}
      <FilterButtonContainer
        filter='SHOW_ACTIVE'
      >
        Active
        </FilterButtonContainer>
      {' '}
      <FilterButtonContainer
        filter='SHOW_COMPLETED'
      >
        Completed
      </FilterButtonContainer>
    </div>
  );
};


const TodoApp = ({ todos, visibilityFilter }) => {
  return (
    <div className='todos-wrapper'>

      <AddTodo
        onAddClick={ (text) => {
          store.dispatch({
            type: 'TODO_ADD',
            id: store.getState().todos.length,
            text,
          })
        } }
      />

      <TodoList
        todos={ getVisibleTodos(todos, visibilityFilter) }
        onTodoClick={ (todoId) => {
          store.dispatch({ type: 'TODO_TOGGLE', id: todoId });
        }}
      />

      <Footer />

    </div>
  );
};

const store = createStore(todoApp);

const render = () => {
  l('store state', store.getState());
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
  id: store.getState().todos.length,
  text: 'Learn Redux',
});
store.dispatch({
  type: 'TODO_ADD',
  id: store.getState().todos.length,
  text: 'Play Tomb Raider I, 1996',
});
store.dispatch({
  type: 'TODO_ADD',
  id: store.getState().todos.length,
  text: 'Study the book HtDP 2e',
});

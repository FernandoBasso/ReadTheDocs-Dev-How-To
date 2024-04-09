/* eslint-disable no-unused-vars */

import expect from 'expect';
import { createStore, combineReducers } from 'redux';
import React, { useRef, useEffect, useState, useContext } from 'react';
import ReactDOM from 'react-dom';
import { Provider, connect } from 'react-redux';

import {
  l,
} from './lib';

import './todoapp.css';

const log = console.log.bind(console);

/**
 * A custom hook to force a re-render of a functional component.
 *
 * The hook returns a single element. Use it like:
 *
 *    const forceUpdate = useForceUpdate();
 *
 * Then, you can use `forceUpdate' when you need to force an update/re-render
 * of a component:
 *
 *    store.subscribe(forceUpdate);
 *
 * Now, every time the store changes, the component will re-render itself.
 *
 * https://stackoverflow.com/questions/46240647/react-how-can-i-force-render-a-function-component
 *
 * @return {function}
 */
const useForceUpdate = () => {
  const [val, setVal] = useState(0);
  return () => setVal(val => ++val);
};



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


const store = createStore(todoApp);

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

let FilterButton = ({
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

const mapStateFilterButtonToProps = (state, ownProps) => {
  return {
    active: ownProps.filter === state.visibilityFilter,
  };
};

const mapDispatchFilterButtonToProps = (dispatch, ownProps) => {
  return {
    onClick: () => {
      dispatch({
        type: 'SET_VISIBILITY_FILTER',
        filter: ownProps.filter,
      });
    },
  };
};

FilterButton = connect(
  mapStateFilterButtonToProps,
  mapDispatchFilterButtonToProps,
)(FilterButton);


let AddTodo = ({ dispatch }) => {
  const inputRef = useRef(null);
  return (
    <div>
      <input ref={inputRef} type='text' />
      <button
        onClick={ () => {
          dispatch({
            type: 'TODO_ADD',
            id: store.getState().todos.length,
            text: inputRef.current.value,
          });
        } }
      >
        Add Todo
      </button>
    </div>
  );
};

AddTodo = connect()(AddTodo);

// const mapFooterStateToProps

let Footer = (props, store) => {
  return (
    <div>
      Filters: {' '}
      <FilterButton filter='SHOW_ALL'>
        All
      </FilterButton>
      {' '}
      <FilterButton filter='SHOW_ACTIVE'>
        Active
      </FilterButton>
      {' '}
      <FilterButton filter='SHOW_COMPLETED'>
        Completed
      </FilterButton>
    </div>
  );
};

Footer = connect()(Footer);

const mapStateTodoListToProps = (state) => {
  const { todos, visibilityFilter } = state;
  return {
    todos: getVisibleTodos(todos, visibilityFilter),
  };
};

const mapDispatchTodoListToProps = (dispatch) => {
  return {
    onTodoClick: (todoId) => {
      dispatch({
        type: 'TODO_TOGGLE',
        id: todoId,
      });
    },
  };
};

const VisibleTodoList = connect(
  mapStateTodoListToProps,
  mapDispatchTodoListToProps,
)(TodoList);


const TodoApp = ({
  store,
}) => {
  // const forceUpdate = useForceUpdate();
  // store.subscribe(forceUpdate);
  return (
    <div className='todos-wrapper'>
      <AddTodo />
      <VisibleTodoList />
      <Footer />
    </div>
  );
};

ReactDOM.render(
  <Provider store={ store }>
    <TodoApp />
  </Provider>,
  document.getElementById('root')
);

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

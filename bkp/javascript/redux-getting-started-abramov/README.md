# Getting Started with Redux - Dan Abramov

By Dan Abramov!

https://egghead.io/courses/getting-started-with-redux


## Running

Run with `npm start`, open the browser at http://localhost:3000 and open the browser console to see some messages.

https://egghead.io/courses/getting-started-with-redux

See the director ``javascript/redux-getting-started-abramov/`` for the full source code.

30 lessons.

Three principles of Redux
-------------------------

<https://redux.js.org/introduction/three-principles/>


* Single source of truth
* State is read-only
* Changes are made with pure functions



01 The Single Immutable State Tree
-----------------------------------------
[Video Lesson 1](https://egghead.io/lessons/react-redux-the-single-immutable-state-tree)

Single Immutable State Tree, a.k.a “__Single Source Of Truth__.”

The whole state of the application is represented with a single (plain) JS object. Every state change is explicit and can be tracked.

Everything that changes in the application, whether the data and/or the UI state, is contained in the single object called the State (or The State Tree).


02 Describing State Changes with Actions
-----------------------------------------------
[Video Lesson 2](https://egghead.io/lessons/react-redux-describing-state-changes-with-actions)

Cannot mutate the state and the State Tree directly. To change the state, an action needs to be dispatched.

An action is a plain js object describing the change.

The state is the minimum representation of the data in the app.

The action is the minimum representation of the change to the data.

The action *must* have a ``type`` property which is not undefined.


03 Pure and Impure Functions
-----------------------------------
[Video Lesson 3](https://egghead.io/lessons/react-redux-pure-and-impure-functions)

If you accidentaly mutate the state tree, redux cannot detect a change has ocurred. Redux compares objects references, not values.


04 The Reducer Function
------------------------------
[Video Lesson 4](https://egghead.io/lessons/react-redux-the-reducer-function)

The state mutation needs to be described as a pure function that takes the previous state and the action being dispatched and returns the next state of the application. This function is called “the reducer.”

05 Writing a Counter Reducer with Tests
----------------------------------------------
[Video Lesson 5](https://egghead.io/lessons/react-redux-writing-a-counter-reducer-with-tests)

When the action type is not provided, or it is unknown, the reducer should return the state unchanged. Not even a copy of it, but the same reference to the State Tree Object so it intentionally *does not* cause a state change. If we returned a copy of the object, it is a new reference, and it would cause redux to perceive it as a state change (because ``{} !== {}`` is true).

The reducer also has to specify the default initial state if it is not provided.

06 Store Methods: getState(), dispatch(), and subscribe()
----------------------------------------------------------------
[Video Lesson 6](https://egghead.io/lessons/react-redux-store-methods-getstate-dispatch-and-subscribe)


* ``getState()``
* ``dispatch()``
* ``subscribe()``



07 Implementing Store from Scratch
-----------------------------------------

[Video Lesson 7](https://egghead.io/lessons/react-redux-implementing-store-from-scratch)

Nice cool JavaScript stuff! Worth it by that reason alone, besides the nice view into ``createStore()``. Very instructive.


08 React Counter Example
-------------------------------
[Video Lesson 8](https://egghead.io/lessons/react-redux-react-counter-example)

A <Counter /> component in React. We do not hard-code the store/redux dependency inside the component. Rather, we pass helper functions as props because it makes testing possible, or at least easier, and because it would allows one to change the implementation of the state should the need arise in the future.


09 Avoiding Array Mutations with concat(), slice(), and ...spread
-----------------------------------------------------------------
[Video Lesson 9](https://egghead.io/lessons/react-redux-avoiding-array-mutations-with-concat-slice-and-spread)

``push()`` and ``splice()`` mutate the object they operate on. Do not use them when you want immutable data structures. Use ``concat()``, ``slice()`` and ``...spread`` instead.

### Add to array
Do NOT do this:

	list.push(0)
	return list;

It mutates ``list``. Do this instead:

	return [...list, 0];

Or this:

	return list.concat([0]);

Using spread or concat creates a new array and *does not* modify the original one.

### Remove from array

Do NOT do this:

	list.splice(idx, 1);
	return list;

It mutates ``list``. Do this instead:

	return list
	  .slice(0, idx)
	  .concat(idx + 1);

That is, create a new list with the elements before and after the element we need to remove. That is, create a new list containing all elements of the original list, except the elements at index ``idx``.

Or with ES6 spread operator:

	return [
	  ...list.slice(0, idx),
	  ...list.slice(idx + 1),
	];


### Increment value of array element

Do NOT do this:

	list[idx]++;
	return list;

The problem is that ``list[idx]++`` mutates ``list``. We do not want to do that. We want to leave ``list`` unmodified, and return a new array state derived from the original one. Do this:

	return list
	  .slice(0, idx)
	  .concat([list[idx] + 1])
	  .concat(list[idx + 1]);

Yes, it is more complex, but we are doing it in an immutable, clean, functional programming stylish way. Also, we are being compliant to Redux requirement of never mutating the state tree.

Or using spread and slicing:

	return [
	  ...list.slice(0, idx),
	  list[idx] + 1,
	  ...list.slice(idx + 1),
	];

The idea is similar as to when removing an item from the array, but instead of ignoring the item to be removed, keep it, incremented by one.

**NOTE**: To decrement an array element, we would use exactly the same logic as increment, except instead of adding one, we subtract one.


10 Avoiding Object Mutations with Object.assign() and ...spread
----------------------------------------------------------------------
[Video Lesson 10](https://egghead.io/lessons/react-redux-avoiding-object-mutations-with-object-assign-and-spread)

Do not mutate objects directly. Create new objects with properties from the original object and any new values needed for properties that should change.

To toggle the property ``compl`` in the object ``todo``, do NOT do this:

	todo.compl = !todo.compl;
	return todo;

That is changing the original object. Do this instead:

	return Object.assign(
	  {},
	  todo,
	  { compl: !todo.compl },
	);

Note that we do not copy the properties one by one. Instead, we use ``Object.assign()`` to copy everything to a new object, and we just manually override what we need. It means if we someday add more properties to the object, this code will still work for toggling a todo.

Or using spread:

	return {
	  ...todo,
	  compl: !todo.compl,
	};

**NOTE**: To copy nested properties, we would have some more work to do as the spread syntax does not do that. [MDN spread docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax). @TODO: reasearch more on this.

**NOTE**: ``Object.assign()`` is ES6, and object spread is ES7.


11 Redux: Writing a Todo List Reducer (Adding a Todo)
-----------------------------------------------------
[Video Lesson 11](https://egghead.io/lessons/react-redux-writing-a-todo-list-reducer-adding-a-todo)

**RULE**: Every reducer has to return the current state for any unknown action.


12 Writing a Todo List Reducer (Toggling a Todo)
-------------------------------------------------------
[Video Lesson 12](https://egghead.io/lessons/react-redux-writing-a-todo-list-reducer-toggling-a-todo)

Two ways to toggle the todo in the switch case. Using an approaching similar to removing a todo:

	const idx = state.findIndex(todo => todo.id === action.id);
	return [
	  ...state.slice(0, idx),
	  { ...state[idx], compl: !state[idx].compl },
	  ...state.slice(idx + 1),
	];

Using map (solution from the video):

	return state.map(todo => {
	  if (todo.id !== action.id) return todo;
	
	  return { ...todo, compl: !todo.compl };
	});


13 Reducer Composition with Arrays
-----------------------------------------
[Video Lesson 13](https://egghead.io/lessons/react-redux-reducer-composition-with-arrays)

The ``todos`` reducer do too many things. It concerns itself with updating the todo's  array *and* how individual todos are updated. Let's create a ``todo`` reducer/function that handles a todo (add, toggle). It will concern itself with individual todos.

This pattern is pervasive in redux development. It is called **reducer composition**.

""
Different reducers specify how different parts of the state tree are updated in response to actions. Reducers are also normal JavaScript functions, so they can call other reducers to delegate and abstract away handling of updates of some parts of the tree they manage.
""

The ``todos`` reducer handles changes in array, and ``todo`` reducer handles changes to individual todo.

""
This pattern can be applied many times, and while there is still a single top level reducer managing the state of your app, you will find it convenient to express it as many reducers calling each other, each contributing to a part of the application state tree.
""

**NOTE**: Read the comments on the [video link](https://egghead.io/lessons/react-redux-reducer-composition-with-arrays).


14 Reducer Composition with Objects
------------------------------------------
[Video Lesson 14](https://egghead.io/lessons/react-redux-reducer-composition-with-objects)

To add new situations to the reducers, do not change the existing one. Rather, create new reducers and use the composition pattern.

Here, we create another reducer, which delegates responsibilities to other reducers.

When an action comes in, it calls the reducers with the part of the state that they manage and the action and combines the results into the new state object.


15 Reducer Composition with combineReducers()
----------------------------------------------------
[Video Lesson 15](https://egghead.io/lessons/react-redux-reducer-composition-with-combinereducers)

Use of Redux's implementation of ``combineReducers()``.


16 Implementing combineReducers() from Scratch
-----------------------------------------------------
[Video Lesson 16](https://egghead.io/lessons/react-redux-implementing-combinereducers-from-scratch)

	const combineReducers = (reducers) => {
	  return (state = {}, action) => {
	    return Object.keys(reducers).reduce((nextState, key) => {
	      nextState[key] = reducers[key](state[key], action);
	      return nextState;
	    }, {});
	  };
	};

The return value of ``combineReducers()`` is a reducer itself. That means, ``combineReducers()`` is a function that returns a function.

``nextState`` is the accumulator of ``Array.prototype.reduce()``. It has the initial value of ``{}``.

The accumulator state *is* modified; it *is* mutated, but it is not a problem because it is an object created *inside* that function. We are not mutating something from outside that came as a parameter. Our function is still considered pure.


17 React Todo List Example (Adding a Todo)
-------------------------------------------------
[Video Lesson 17](https://egghead.io/lessons/react-redux-react-todo-list-example-adding-a-todo)




18 React Todo List Example (Toggling a Todo)
---------------------------------------------------
[Video Lesson 18](https://egghead.io/lessons/react-redux-react-todo-list-example-toggling-a-todo)




19 React Todo List Example (Filtering Todos)
---------------------------------------------------
[Video Lesson 19](https://egghead.io/lessons/react-redux-react-todo-list-example-filtering-todos)



20 Extracting Presentational Components (Todo, TodoList)
---------------------------------------------------------------
[Video Lesson 19](https://egghead.io/lessons/react-redux-extracting-presentational-components-todo-todolist)

Separating Presentational Components from Container Components helps with separation of logic and concerns, while it also makes it more complex in terms of following the code, and passing props left and right all over the place. That can be mitigated with techniques shown in the next videos.


21 Extracting Presentational Components (AddTodo, Footer, FilterLink)
----------------------------------------------------------------------------
[Video Lesson 21](https://egghead.io/lessons/react-redux-extracting-presentational-components-addtodo-footer-filterlink)



22 Extracting Container Components (FilterLink)
------------------------------------------------------
[Video Lesson 22](https://egghead.io/lessons/react-redux-extracting-container-components-filterlink)

With the previous approach, we have to pass props down the tree of components, even when the intermediate components don't use those props (just pass them further along).
For example, ``<FooterLink />`` component has to receive the ``visibilitFilter`` prop just to pass it along to ``<FilterLink />``.

The parent components have to know too much data that the child components need.

A Container Component provides the behavior and the data to the Presentational Component.



## 23 Extracting Container Components (VisibleTodoList, AddTodo)

[Video Lesson 23](https://egghead.io/lessons/react-redux-extracting-container-components-visibletodolist-addtodo)

> All container components are similar. Their job is to connect a  presentational component to the Redux store and specify the data and the behavior that it (the presentational component) needs.
>
> -- Dan Abramov



## 24 Passing the Store Down Explicitly via Props

[Video Lesson 24](https://egghead.io/lessons/react-redux-passing-the-store-down-explicitly-via-props)

Passing store down, manually, explicitly, via props makes testing possible, but it does not scale well because we need to pass it all over the place.



## 25 Passing the Store Down Implicitly via Context

[Video Lesson 25](https://egghead.io/lessons/react-redux-passing-the-store-down-implicitly-via-context)



## 26 Passing the Store Down with `<Provider>` from React Redux

[Video Lesson 26](https://egghead.io/lessons/react-redux-passing-the-store-down-with-provider-from-react-redux)

Did not work well with newer React versions and using functional components and hooks.



## 27 Generating Containers with connect() from React Redux (VisibleTodoList)

[Video Lesson 27](https://egghead.io/lessons/react-redux-generating-containers-with-connect-from-react-redux-visibletodolist)



## 28 Generating Containers with connect() from React Redux (AddTodo)

[Video Lesson 28](https://egghead.io/lessons/react-redux-generating-containers-with-connect-from-react-redux-addtodo)



## 29 Generating Containers with connect() from React Redux (FooterLink)

[Video Lesson 29](https://egghead.io/lessons/react-redux-generating-containers-with-connect-from-react-redux-footerlink)



## 30 Extracting Action Creators

[Video Lesson 30](https://egghead.io/lessons/react-redux-extracting-action-creators)


















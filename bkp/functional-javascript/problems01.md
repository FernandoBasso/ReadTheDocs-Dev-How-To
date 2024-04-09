# Problems 01

- [add numbers from a list](#add-numbers-from-a-list)
  - [traditional approach](#traditional-approach)
  - [functional style](#functional-style)
  - [functional style v2](#functional-style-v2)
- [filter todos](#filter-todos)

## add numbers from a list

### traditional approach

Using a more traditional approach.

```js
// add :: [Number] -> Number
const add = nums => {
  let total = 0;
  for (let i = 0; i < nums.length; ++i) {
    total += nums[i];
  };

  return total;
}

log(add([1, 2, 3, 4]));
// → 10
```

### functional style

And what about this really functional approach? We create our `add` function _on the fly_, with the `(acc num) => acc + num` thing.

```js
const { reduce } = require('ramda');

// add :: [Number] -> Number
const addNums = nums => reduce((acc, num) => acc + num, 0, nums);

log(addNums([1, 2, 3, 4]));
// → 10
```

### functional style v2

In this case, instead of using our _on the fly add function_, we use the `add` function provided by rambda.

```js
const { reduce, add } = require('ramda');

// add :: [Number] -> Number
const addNums = nums => reduce(add, 0, nums);

log(addNums([1, 2, 3, 4]));
// → 10
```

## filter todos

```js
const {
  filter,
  where,
  equals: eq,
  complement,
} = require('ramda');

// task: { id: number, done: boolean, text: string, due: string yyyy-mm-dd }

// tasks: [todo]
const tasks = [
  { id: 1, user: 'Yoda', done: false, text: 'Watch the Alien Movies Again', due: '2000-01-01' },
  { id: 2, user: 'Luke', done: true, text: 'Learn JavaScript', due: '2000-01-03' },
  { id: 3, user: 'Luke', done: false, text: 'Learn They Hindley Milner Type System', due: '2000-01-01' },
  { id: 4, user: 'Yoda', done: false, text: 'Review Sed Exercises', due: '2000-01-02' },
  { id: 5, user: 'Leia', done: true, text: 'Update Arch Linux', due: '2000-01-03' },
];

// isFinished :: Task -> Bool
const isFinished = where({ done: eq(true)} ); // <1>

// isUnfinished :: Task -> Bool
const isUnfinished = complement(isFinished); // <2>

// getFinishedTasks :: [Task] -> [Task]
const getFinishedTasks = filter(isFinished); // <3>
log(getFinishedTasks(tasks));
// Prints all tasks with `done: true`.

// getUnfinishedTasks :: [Task] -> [Task]
const getUnfinishedTasks = filter(isUnfinished); // <4>
log(getUnfinishedTasks(tasks));
// → Prints all tasks with `done: false`.
```

1. When we supply a task to `isFinished`, it will check whether it is done or not.
2. Same as for `isFinished`. But we use `complement` to invert the logic.
3. Yes, we curry all things \o/ - `isFinished` needs a task, which `filter` takes care of passing. `filter` needs a list of tasks, which we pass when we invoke `getFinishedTasks`.
4. Same idea as item 3.






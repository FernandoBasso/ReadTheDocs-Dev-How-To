# Partial Application and Currying

- [filtering a book list by date](#filtering-a-book-list-by-date)
- [an improved function](#an-improved-function)

## filtering a book list by date

```js
const { filter, map, prop} = require('ramda');

const bookList = [
  { title: 'Lord of The Rings', year: 1954 },
  { title: 'The Silence Of The Lambs', year: 1988 },
  { title: 'The Little Prince', year: 1943 },
];

/**
 * Produce `true` if the book `year` is the same as `targetYear`.
 *
 * Curried!
 *
 * publishedInYear :: Year -> Book -> Boolean
 */
const publishedInYear = targetYear => ({ year }) => year === targetYear;


/**
 * Produce a list of titles of books published on a given year.
 *
 * titlesForyear :: List-Of-Book, Year -> List-Of-Book
 */
const titlesForyear = (targetYear, books) => {
  const selectedBooks = filter(publishedInYear(targetYear), books); // <1>
  return map(prop('title'), selectedBooks); // <2>
};

log(titlesForyear(1988, bookList));
// → [ 'The Silence Of The Lambs' ]
```

1. Since `publishedInYear` is curried, we pass it `targetYear`, and `filter` takes care of passing it a `book`, the “missing” argument.
2. `map` takes care of passing the `book` to `prop`. We only pass `prop` the property we want to retrieve.

### an improved function

It is possible to make `titlesForYear` even more functional-like by making use of more currying and Ramda's `pipe` funciton.

```js
/**
 * Produce a list of titles of books published on a given year.
 *
 * titlesForyear :: List-Of-Book, Year -> List-Of-Book
 */
const titlesForyear = (targetYear, books) => pipe(
  filter(publishedInYear(targetYear)), // <1>
  map(prop('title')), // <2>
)(books); // <3>

log(titlesForyear(1988, bookList));
// → [ 'The Silence Of The Lambs' ]
```

1. `pipe` handles passing the `books` data to `filter`, and `filter` passes the `book` data to `publishedInYear`.
2. `pipe` passes the `books` data to `map`, and `map` in turn passes the `book` data to `prop`.
3. By passing `books` to `pipe`, `pipe` knows what data to pass to `filter` and `map`.

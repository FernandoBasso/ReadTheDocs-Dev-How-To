export const NAME = "generator functions v1";

const log: Console["log"] = console.log.bind(console);

//
// This is the inferred return type for the generator function:
//
//   Generator<1 | 2 | 3 | 4, string, unknown>
//

function* numsGenerator() {
  yield 1;
  yield 2;

  let proceed: unknown = yield 3;

  if (proceed) yield 4;

  log("inside:", { shouldContinue: proceed });

  return "done";
}

//
// People call the variable used to store the iterator
// ‘gen<something>, but ‘it<something>’, or ‘iterator<something>’
// would likely be more appropriate, since calling the generator
// function returns an iterator...
//
const numsIterator: Generator<1 | 2 | 3 | 4 | string | unknown> = numsGenerator();

log(numsIterator.next().value);
// → 1

log(numsIterator.next().value);
// → 2

log(numsIterator.next().value);
// → 3

//
// At this point, given our
//
log(numsIterator.next(true).value);
// → 4

log(numsIterator.next().value);
// → done

//
// A generator function returns an iterator when it is called.
// That iterator then can be use to fetch more and more values
// (of course, depending on the implementation of the generator
// function).
//

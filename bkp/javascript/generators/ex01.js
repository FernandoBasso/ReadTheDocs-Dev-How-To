const l = console.log.bind(console);

// ‘function*’ is a keyword. ‘yield’ is an operator.

function* genFn() {
  l('First');
  yield;
  l('Second');
}

// Calling a generator function returns a generator object that does
// NOT immediately invoke the first actual line of code inside the
// body of the function.

const genObj = genFn();

const inv1 = genObj.next();
const inv2 = genObj.next();

// Invoking .next() prints the logs, but each time, the generator
// object returns an object of the form:
//
//  { value: <something>, done: true/false }

// ‘yield’ is an operator, just like ‘typeof’ is an operator in
// JavaScript (‘typeof’ is an operator in C too, and in many other
// languages).

// ‘genObj.next()’ executes the next available ‘yield’ and returns
// an object ‘{ value: <some value>, done: true/false }’.

l(inv1);
l(inv2);

// ‘inv1’ and ‘inv2’ have ‘value: undefined’ because ‘yield’ in
// this example did not take any value as param, so it defaults to
// ‘undefined’.

/* vim: set tw=68: */


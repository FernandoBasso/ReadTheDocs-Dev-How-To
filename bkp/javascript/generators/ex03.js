const l = console.log.bind(console);

function* genFn() {
  yield 'Master';
  yield 'Yoda';
}

const genObj = genFn();

l(genObj.next());
l(genObj.next());
l(genObj.next());

// The generator yields twice, but it is done only on the third .next().

/* vim: set tw=68: */


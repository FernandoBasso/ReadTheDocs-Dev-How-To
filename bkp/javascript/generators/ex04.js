const l = console.log.bind(console);

function* genFn() {
  yield 'Tomb';
  yield 'Raider';
  return 1996;
}

l(typeof genFn, genFn.constructor, genFn.constructor === Function);

// const genObj = genFn();
// const next = genObj.next.bind(genObj);

function extractNext(fn) {
  const o = fn();
  return o.next.bind(o);
}

const next = extractNext(genFn);

l(next());
l(next());
l(next());

/* vim: set tw=68: */


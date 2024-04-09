const l = console.log.bind(console);

function* genFn() {
  yield
}

function isGeneratorFn(f) {
  const o = f();
  // Test!
  l(o[Symbol.iterator], o[Symbol.iterator].constructor === Function);
  l(typeof f()[Symbol.iterator]);

}

isGeneratorFn(genFn);

/* vim: set tw=68: */


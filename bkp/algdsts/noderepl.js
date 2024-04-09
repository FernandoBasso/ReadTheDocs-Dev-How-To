import repl from 'repl';

[
  'log',
  'log10',
  'abs',
  'floor',
  'ceil',
  'round',
  'trunc',
  'pow',
].forEach(function addToGlobal (fn) {
  global[fn] = Math[fn].bind(Math);
});

repl.start();

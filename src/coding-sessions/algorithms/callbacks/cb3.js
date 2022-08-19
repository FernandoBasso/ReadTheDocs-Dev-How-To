const log = console.log.bind(console);

function run(g) {
  return g();
}

function fn() {
  return 1;
}

var h = run(fn);

var i = run(fn());
// run(1)
// 1();

function cb() {
  log(10);
  // return undefined;
}

var k = run(cb());

/*
cb() -> undefined

run(undefined);

function run(g) {
  return g();
}

replace g with undefined

function run(undefined) {
  return undefined();
}
*/

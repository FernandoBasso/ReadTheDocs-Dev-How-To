const log = console.log.bind(console);

var x = 1;

function f() {
  return x + 1
}

log(f());

//
// Which `x` will be remembered by the closure created by `f`?
//
// It will be the second one. The output is 2.
//

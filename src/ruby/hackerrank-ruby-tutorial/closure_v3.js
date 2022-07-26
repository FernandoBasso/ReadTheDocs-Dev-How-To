const log = console.log.bind(console);

var x = 1;

function f() {
  return x + 1
}

var x = 10;

log(f());

//
// Which `x` will be remembered by the closure created by `f`?
//
// It will be the second one. The output is 11.
//
// Because of hoisting, this is different than closure_v3.rb
// which would cause an error here saying `x` is not defined.
//

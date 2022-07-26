const log = console.log.bind(console);

var x = 1;

function f() {
  return x + 1
}

log(f());

//
// `x` will be remembered by the closure and the output is 2.
//

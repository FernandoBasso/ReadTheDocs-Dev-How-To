const log = console.log.bind(console);

// var x = 1;

function f() {
  return x + 1
}

var x = 10;

log(f());

//
// Because of hoisting, this is different than closure_v2.rb
// which would cause an error here saying `x` is not defined.
//
// In JavaScript, it will print 11 and not an error, like in
// closure_v2.rb.
//

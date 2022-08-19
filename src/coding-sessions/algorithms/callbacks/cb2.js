const log = console.log.bind(console);

function run(g) {
  return g();
}

function add(x, y) {
  return x + y;
}

function fn() {
  log(10);
  return undefined;
};

run(fn);


var h = add(1, 2);

var i = fn();

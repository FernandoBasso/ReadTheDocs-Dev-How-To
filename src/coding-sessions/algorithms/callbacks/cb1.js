const log = console.log.bind(console);

function run(g) {
  return g();
}

function fn() {
  log(10);
};

run(fn);


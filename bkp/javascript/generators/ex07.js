const l = console.log.bind(console);

function* range(max = 8) {
  for (let i = 0; i <= max; i += 2) {
    yield i;
  }
}

l([...range(14)]);

/* vim: set tw=68: */


const l = console.log.bind(console);

// Generators can produce iterables, which can be consumed by:
// - ‘for-of’
// - spread operator ‘...’
// - destructuring

function* objEntries(obj) {
  const keys = Reflect.ownKeys(obj);

  for (const key of keys) {
    // Produces a key/val tuple-like array thing.
    yield [key, obj[key]]
  }
}

const yoda = {
  id: 1,
  name: 'Yoda',
  skill: 'The Force',
};

// Spread consumes the entire iterator in a single fell swoop. The
// producer ‘objEntries’ stops when there is nothing else to
// produce.
l(...objEntries(yoda));

const [id, name, skill] = objEntries(yoda);
l(id, name, skill);


// Note the destructuring of the tuple into ‘k’ and ‘v’ variables.
for (const [k, v] of objEntries(yoda)) {
  l(`${k}: ${v}`);
}

/* vim: set tw=68: */


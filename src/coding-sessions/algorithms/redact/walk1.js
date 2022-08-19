const log = console.log.bind(console);

function isObj(o) {
  return o !== null && typeof o === "object";
}

var o1 = {
  headers: {
    foo: 'x-foo',
    bar: 'x-bar',
  }, 
  data: {
    id: 'x-id',
    items: {
      item1: 'x-1item',
      item2: 'x-2item',
      thing: {
        jedis: [
          { yoda: 'Master Yoda' },
          { obi: 'Obi-wan Kenobi' },
          { luke: 'Luke Skywalker'} ,
        ],
      },
    },
  },
};

const KEYS_TO_REDACT = [
  "obi",
  "item2",
];

function redacter(s) {
  return s.slice(0, 4).toUpperCase();
}

function walk(o, f) {
  let dup = {};
  let val;

  for (const k in o) {
    v = o[k];

    dup[k] = isObj(v)
      ? walk(v, f)
      : KEYS_TO_REDACT.includes(k) ? f(v) : v;
  }

  return dup;
}

log(JSON.stringify(walk(o1, redacter), null, 2));
log(o1);

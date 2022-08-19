const log = console.log.bind(console);



const KEYS_TO_TRIM = [
  "foo",
  "bar",
];

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

// var res = walkObj(o1, {});
// log(res);

function redact(s) {
  return s.slice(0, 4).toUpperCase();
}

function walk1(obj) {
  let val;

  for (const key in obj) {
    val = obj[key];

    obj[key] = isObj(val)
      ? walk(val)
      : redact(val);
  }

  return obj;
}

function walk(obj) {
  let dup = {};
  let val;

  for (const key in obj) {
    val = obj[key];

    dup[key] = isObj(val)
      ? walk2(val)
      : redact(val);
  }

  return dup;
}


log(JSON.stringify(walk2(o1), null, 2));
log(o1);

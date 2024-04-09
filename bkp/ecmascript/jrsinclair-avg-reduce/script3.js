
//
// https://jrsinclair.com/articles/2019/five-ways-to-average-with-js-reduce/
//

const l = console.log.bind(console, '→');

const victorianSlang = [
  {
    term: 'doing the bear',
    found: true,
    popularity: 108,
  },
  {
    term: 'katterzem',
    found: false,
    popularity: null,
  },
  {
    term: 'bone shaker',
    found: true,
    popularity: 609,
  },
  {
    term: 'smothering a parrot',
    found: false,
    popularity: null,
  },
  {
    term: 'damfino',
    found: true,
    popularity: 232,
  },
  {
    term: 'rain napper',
    found: false,
    popularity: null,
  },
  {
    term: 'donkey’s breakfast',
    found: true,
    popularity: 787,
  },
  {
    term: 'rational costume',
    found: true,
    popularity: 513,
  },
  {
    term: 'mind the grease',
    found: true,
    popularity: 154,
  },
];


// -----------------------------------------------------------------------------
// Helper Functions
const filter = p => xs => xs.filter(p);
const map = f => xs => xs.map(f);
const prop = k => o => o[k];
const reduce = f => acc => xs => xs.reduce(f, acc);
const compose = (...fns) => arg => fns.reduceRight((arg, fn) => fn(arg), arg);

// Read about Blackbird Combinator.
// https://jrsinclair.com/articles/2019/compose-js-functions-multiple-parameters/
const B1 = f => g => h => x => f(g(x))(h(x));

// -----------------------------------------------------------------------------
// Calculations

const sum = reduce((x, acc) => acc + x)(0);

const length = xs => xs.length;

const div = x  => y => x / y;

const calcuPopularity = compose(
  B1(div)(sum)(length),
  map(prop('popularity')),
  filter(prop('found')),
);

l('Average Popularity:', calcuPopularity(victorianSlang));

/*

“It’s possible to build our average-calculation function using only compose();
with no variables. We call this style ‘point-free’, or ‘tacit’ programming.
But to make it work, we need a lot of helper functions.”

*/


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
const isFound = (item) => item.found;

const getPopularity = (item) => item.popularity;

const addScores = (runningTotal, popularity) => runningTotal + popularity;

// -----------------------------------------------------------------------------
// Calculations

const terms = victorianSlang.filter(isFound);

const popularityScores = terms.map(getPopularity);

const scoresTotal = popularityScores.reduce(addScores, 0);

const average = scoresTotal / popularityScores.length;

l('Average Popularity:', average);

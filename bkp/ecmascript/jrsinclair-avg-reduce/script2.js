
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

// Use an object to track multiple values in a single return value.
const addScores = ({ totalPopularity, itemCount }, popularity) => {
  return {
    totalPopularity: totalPopularity + popularity,
    itemCount: itemCount + 1,
  };
};

// -----------------------------------------------------------------------------
// Calculations

const initialInfo = { totalPopularity: 0, itemCount: 0 };

const popularityInfo = victorianSlang.filter(isFound)
  .map(getPopularity)
  .reduce(addScores, initialInfo);

const averagePopularity = popularityInfo.totalPopularity / popularityInfo.itemCount;

l('Average Popularity:', averagePopularity);


const l: Function = console.log.bind(console);

const xs: Array<number> = [1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15];

const withinRange = (low: number, high: number) => {
  return (val: number): boolean => {
    return val >= low && val <= high;
  };
};

const isWithin: (low: number, high: number) => (val: number) => boolean =
  (low, high) => (val) => val >= low && val <= high;

function filterRange(min: number, max: number, list: number[]): number[] {
  return list.filter(withinRange(min, max), list);
}

const res1: number[] = filterRange(6, 10, xs);
const res2: number[] = xs.filter(isWithin(6, 9));

l(res1);
l(res2);



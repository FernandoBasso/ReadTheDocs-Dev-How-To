import {
  countFreqs,
  getRatios,
} from './plusMinus-v1.js';

describe('countFreqs()', () => {
  test('empty array', () => {
    expect(countFreqs([])).toEqual({
      negatives: 0,
      zeroes: 0,
      positives: 0,
    });
  });

  test('a few negatives', () => {
    expect(countFreqs([-7, -1, -3])).toEqual({
      negatives: 3,
      zeroes: 0,
      positives: 0,
    });
  });

  test('a few zeroes', () => {
    expect(countFreqs([0, 0])).toEqual({
      negatives: 0,
      zeroes: 2,
      positives: 0,
    });
  });

  test('a few positives', () => {
    expect(countFreqs([7, 1, 3, 19])).toEqual({
      negatives: 0,
      zeroes: 0,
      positives: 4,
    });
  });

  test('negatives, zeroes and positives', () => {
    expect(countFreqs([-7, -1, 0, 5, 3, 7, 9])).toEqual({
      negatives: 2,
      zeroes: 1,
      positives: 4,
    });
  });
});

describe('getRatios()', () => {
  test('a few negatives', () => {
    expect(getRatios([-7, -1, -3])).toEqual({
      negatives: 3/3,
      zeroes: 0/3,
      positives: 0/3,
    });
  });

  test('a few zeroes', () => {
    expect(getRatios([0, 0])).toEqual({
      negatives: 0/2,
      zeroes: 2/2,
      positives: 0/2,
    });
  });

  test('a few positives', () => {
    expect(getRatios([7, 1, 3, 19])).toEqual({
      negatives: 0/4,
      zeroes: 0/4,
      positives: 4/4,
    });
  });

  test('negatives, zeroes and positives', () => {
    expect(getRatios([-7, -1, 0, 5, 3, 7, 9])).toEqual({
      negatives: 2/7,
      zeroes: 1/7,
      positives: 4/7,
    });
  });
});

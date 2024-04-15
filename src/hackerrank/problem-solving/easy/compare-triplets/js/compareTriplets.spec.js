const { compareTriplets } = require('./compareTriplets-v6');

describe('compareTriplets()', () => {
  it('should work when alice wins', () => {
    const aliceRating = [17, 28, 30];
    const bobRating = [99, 16, 8];

    const expectedPoints = [2, 1];

    expect(
      compareTriplets(aliceRating, bobRating)
    ).toEqual(expectedPoints);
  });

  it('should work when bob wins', () => {
    const aliceRating = [99, 16, 8];
    const bobRating = [17, 28, 30];

    const expectedPoints = [1, 2];

    expect(
      compareTriplets(aliceRating, bobRating)
    ).toEqual(expectedPoints);
  });

  it('should work when there is a tie', () => {
    const aliceRating = [17, 16, 30];
    const bobRating = [99, 16, 8];

    const expectedPoints = [1, 1];

    expect(
      compareTriplets(aliceRating, bobRating)
    ).toEqual(expectedPoints);
  });

  it('should work when all ratings are a tie', () => {
    const aliceRating = [17, 16, 8];
    const bobRating = [17, 16, 8];

    const expectedPoints = [0, 0];

    expect(
      compareTriplets(aliceRating, bobRating)
    ).toEqual(expectedPoints);
  });
});

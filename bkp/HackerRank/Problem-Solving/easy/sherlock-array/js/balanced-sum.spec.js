import { balancedSum } from './balanced-sum-v2.js';

//
// • design tool
// • documentation
// • unit test
// • regression test
//

//
// NOTE: Our solution does not work with Infinity.
//

describe('balancedSum(xs)', () => {
  test('array with 1 element is always balanced', () => {
    expect(balancedSum([7])).toBe('YES');
  });

  test('array with two elements', () => {
    expect(balancedSum([3, 7])).toBe('NO');
    expect(balancedSum([0, 0])).toBe('YES');
    expect(balancedSum([1, 0])).toBe('YES');
    expect(balancedSum([0, 1])).toBe('YES');
    // expect(balancedSum([Infinity, 0])).toBe('YES');
    // expect(balancedSum([0, -Infinity, Infinity, Infinity, 0, -Infinity, Infinity])).toBe('YES');
  });

  test('array with three elements', () => {
    expect(balancedSum([1, 1, 1])).toBe('YES');
    expect(balancedSum([-1, 1, 1])).toBe('YES');
    // expect(balancedSum([Infinity, 1, 1, 1, Infinity])).toBe('YES');
    // expect(balancedSum([-Infinity, 1, 1, 1, -Infinity])).toBe('YES');
    // expect(balancedSum([-Infinity, 1, 1, 1, Infinity])).toBe('NO');
    // expect(balancedSum([Infinity, 1, 1, 1, -Infinity])).toBe('NO');

    expect(balancedSum([1, 2, 3])).toBe('NO');
  });

  test('array with five elements', () => {
    expect(balancedSum([3, 0, 9, 1, 7])).toBe('NO');
    expect(balancedSum([10, -3, -2, 1, 5])).toBe('YES');
  });
});

// import { applyDiscount } from './apply-discount-v1';
import { applyDiscount } from './apply-discount-v2';

describe('applyDiscount()', () => {
  [
    ['$100', '20%', 80],
    ['$50', '5%', 47.5],
    ['$3.14', '0%', 3.14],
    ['$3.14', '100%', 0],
  ].forEach(([price, discount, total]) => {
    it(`should dicount ${discount} from ${price}`, () => {
      expect(applyDiscount(price, discount)).toEqual(total);
    });
  });
});

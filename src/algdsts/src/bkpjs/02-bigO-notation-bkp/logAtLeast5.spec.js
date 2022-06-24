import { jest } from '@jest/globals';
import { logAtLeast5 } from './logAtLeast5-v1';

globalThis.console = {
  log: jest.fn(),
};

beforeEach(() => {
  jest.spyOn(globalThis.console, 'log').mockImplementation(() => jest.fn());
});

describe("logAtLeast5()", () => {
  describe("when the input is less than or equal to 5", () => {
    [1].forEach((num) => {
      it(`for number ${num} it should log exactly 5 times`, () => {
        logAtLeast5(num);
        expect(globalThis.console.log).toHaveBeenCalledTimes(5);
        jest.clearAllMocks(); // <1>
      });
    });
  });

  describe("when the input is more than 5", () => {
    [6, 9, 42].forEach((num) => {
      it(`for number ${num} it should log exactly ${num} times`, () => {
        logAtLeast5(num);
        expect(globalThis.console.log).toHaveBeenCalledTimes(num);
        jest.clearAllMocks();
      });
    });
  });
});

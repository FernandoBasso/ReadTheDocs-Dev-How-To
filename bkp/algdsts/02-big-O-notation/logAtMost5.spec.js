import { jest } from '@jest/globals';
import { logAtMost5 } from './logAtMost5-v1';

beforeEach(() => {
  jest.spyOn(globalThis.console, "log").mockImplementation(() => null);
});

describe("logAtMost5()", () => {
  describe("when the input is more than 5", () => {
    [6, 9, 42].forEach((num) => {
      it(`for number ${num} it should log exactly 5 times`, () => {
        logAtMost5(num);
        expect(globalThis.console.log).toHaveBeenCalledTimes(5);
        jest.clearAllMocks();
      });
    });
  });

  describe("when the input is lest than 5 or equal to 5", () => {
    [0, 3, 5].forEach((num) => {
      it(`for number ${num} it should log exactly ${num} times`, () => {
        logAtMost5(num);
        expect(globalThis.console.log).toHaveBeenCalledTimes(num);
        jest.clearAllMocks();
      });
    });
  });
});

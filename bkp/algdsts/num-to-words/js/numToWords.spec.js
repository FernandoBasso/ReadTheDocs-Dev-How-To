import { numToWords } from "./numToWords-v4";

describe("toWords()", () => {
  it("should convert single number words (7, 1, 9, 0)", () => {
    expect(numToWords(1)).toEqual("one");
  });

  it("should convert two-digit number to words (10)", () => {
    expect(numToWords(10)).toEqual("ten");
  });

  it("should work multiples of ten (20, 30, 90)", () => {
    expect(numToWords(20)).toEqual("twenty");
  });

  it("should work for tens and ones (21, 99)", () => {
    expect(numToWords(22)).toEqual("twenty two");
    expect(numToWords(37)).toEqual("thirty seven");
  });

  it("should work for three-digit numbers (121, 999)", () => {
    expect(numToWords(121)).toEqual("one hundred and twenty one");
  });

  it("should work for three-digit numbers ending in 10s (120, 790)", () => {
    expect(numToWords(120)).toEqual("one hundred and twenty");
    expect(numToWords(970)).toEqual("nine hundred and seventy");
  });

  it("should work for multiples of one hundred (100, 700)", () => {
    expect(numToWords(100)).toEqual("one hundred");
  });
});

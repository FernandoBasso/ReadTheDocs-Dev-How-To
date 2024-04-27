import { compose } from "./compose_v2";

describe("compose()", () => {
  it("is the identity function when fns is empty", () => {
    expect(compose([])(1)).toEqual(1);
  });

  it("composes a single function", () => {
    const add1 = n => n + 1;

    expect(compose([add1])(0)).toEqual(1);
  });

  it("composes from right to left", () => {
    const add1 = n => n + 1;
    const double = n => 2 * n;
    const square = n => n * n;

    //   add1(3) = 4
    // double(4) = 8
    // square(8) = 64
    expect(compose([square, double, add1])(3)).toEqual(64);

    // square(1) = 1
    //   add1(1) = 2
    // double(4) = 4
    expect(compose([square, double, add1])(3)).toEqual(64);
  });
});

import { log } from "./utils";

/** @param x The value.
 * @param y The other value.
 * @returns The sum of the numbers.
 */
const simpleAdd = (x: number, y: number): number => x + y;
const test00 = simpleAdd(4, 6);
log(test00);
// → 10

const curriedAdd =
  (x: number) =>
  (y: number): number =>
    x + y;

const test01: number = curriedAdd(4)(2);
const test02: Function = curriedAdd(15);
const test03: number = test02(5);

log(test01, test02, test03);
// → 6
// → Function (anonymous)
// → 20

type tuple = ["a", number, string[]];

const test04: tuple = ["a", 1, ["w", "z"]];

const [letter, num, strs] = test04;

const test05 = (...args: tuple): boolean => true;
const test06 = test05("a", 42, []);

//////////////////////////////////////////////////////////////////////////////
const fn00 = (name: string, age: number, single: boolean): boolean =>
  true;

type test07 = Parameters<typeof fn00>;

/**
 * Does *the* best **Possible** thing!
 *
 * @param n The input number.
 * @param The result.
 */
function fn(n: number) {
  return n + 1;
}

/*
unknown doesn't work here.
Type '(name: string, age: number, single: boolean) => boolean' does not
satisfy the constraint '(...args: unknown[]) => unknown'.
  Types of parameters 'name' and 'args' are incompatible.
      Type 'unknown' is not assignable to type 'string'.
*/
type Params<F extends (...args: any[]) => any> = F extends (
  ...args: infer A
) => any
  ? A
  : never;

// TheType is unsued, but it is NOT greyed out.
type TheType = Params<typeof fn00>;

////////////////////////////////////////////////////////////////////////
// HEAD
//
type Head<T extends unknown[]> = T extends [unknown, ...unknown[]]
  ? T[0]
  : never;

type test09 = Head<[1, 2, string, number]>;
type test10 = Head<Params<typeof fn00>>;

////////////////////////////////////////////////////////////////////////
// TAIL
//
// Why can't we use unknown?
type Tail<T extends any[]> =
  ((...t: T) => any) extends ((_: any, ...tail: infer TT) => any)
  ? TT
  : [];

type test11 = Tail<[1, 2, string, number]>;

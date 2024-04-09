export const MODNAME = "e10a-unwrap-types";

type RetPromise1 = Promise<Promise<Promise<{ gold: 1e3 }>>>;
// ⇒ type RetPromise1 = Promise<Promise<Promise<{
// ⇒     gold: 1e3;
// ⇒ }>>>
//
// The type is wrapped in multiple promise layers.
////

////
// And there exists a built-in TS utility to unpack the type
// nested inside promises.
//
type RetPromise2 = Awaited<Promise<Promise<Promise<{ gold: 1e3 }>>>>;
//   ^?
// ⇒ type RetPromise2 = {
// ⇒   gold: 1e3;
// ⇒ }
//
// The type is now unwrapped.
////

////
// And we can create some unwrap utils for some other
// situations.
//
type UnwrapArray<T> =
  T extends Array<infer Inner>
    ? UnwrapArray<Inner>
    : T;

type U = Array<Array<number>>;
// ⇒ type U = number[][]

////
// Now we can unwrap the type of the array elements.
//
type V = UnwrapArray<Array<Array<number>>>;
// ⇒ type V = number
////

//
// If we have a value:
//
const arrArrNum = [[1]];
type TArrArrNum = UnwrapArray<typeof arrArrNum>;
// ⇒ type TArrArrNum = number

//
// Or even something like:
//
const arrArrArrOne = [[[1 as const]]];
type TArrArrArrOne = UnwrapArray<typeof arrArrArrOne>;
// ⇒ type TArrArrArrOne = 1

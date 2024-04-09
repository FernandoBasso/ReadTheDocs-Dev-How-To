export const MODNAME = "e11c-tuples";

//
// Let's create a type that can count the tuple elements for us
// and ensure the length is satisfied.
//

//
// 999 is the hard-limit on the number of recursion on the type as
// of 2024 and TypeScript 5.x.
//
type NTuple<
  Length extends number,
  Elem,
  Acc extends Elem[] = []
> = Acc["length"] extends Length
  ? Acc
  : NTuple<Length, Elem, [...Acc, Elem]>;
  //                      Add Elem to the previous added accumulator.
  //                      It is like counting now many Elem's were
  //                      added to the accumulator.

//
// We want Rgb to be a tuple what holds three numbers.
//
type Rgb = NTuple<3, number>;

const green: Rgb = [0, 255];
// ~Type '[number, number]' is not assignable to type
// '[number, number, number]'.

const blue: Rgb = [0, 0, 255];
// Type-checks!

type Tuple42 = NTuple<42, string>;
// type Tuple42 = [string, string ... (42 times in total)]

type TupleStringOrNumber = NTuple<2, string | number>;
// type TupleStringOrNumber = [string | number, string | number]


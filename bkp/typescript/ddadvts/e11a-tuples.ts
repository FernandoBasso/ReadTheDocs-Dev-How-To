export const MODNAME = "e11a-tuples";

//
// By definitions, in TypeScript tuples are arrays with
// known types and length.
//

type Rgb = [number, number, number];

const blue: Rgb = [0, 0, 255];

//
// Even though the three-tuple Rgb is supposed to have exactly three
// elements, we can append another one and TS checker doesn't seem
// to care.
//
blue.push(0);

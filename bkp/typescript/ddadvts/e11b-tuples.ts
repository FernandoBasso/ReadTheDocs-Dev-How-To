export const MODNAME = "e11b-tuples";

//
// Note we added ‘readonly’ now.
//
type Rgb = readonly [number, number, number];

const blue: Rgb = [0, 0, 255];

//
// And then TS checker says there is no ‘push’ (or any other related
// method on `blue`). That means the only way to assign values is
// during the initial assignment.
//
blue.push(0);

//
// And even during assignment it counts the number of the tuple
// elements and type-checks even that for us.
//
const red: Rgb = [255, 0, 0, 0];
// ~ Type '[number, number, number, number]' is not assignable to
// ~ type 'Rgb'. Source has 4 element(s) but target allows only 3.

export const NAME = "l23a Moving in the Type Space";

const log: Console["log"] = console.log.bind(console);

//
// intersection types: a way to combine two or more types into
// one, much like extending from an object type.
//
// union types: a way to extract the lowest common denominator
// of a set of types.
//

//
// Set Theory
// ==========
//
//
// data type
// ---------
//
// A type is a classification of data that defines the
// operations that can be performed on that data, the meaning
// of the data, and the set of allowed values.
//
// ‘any’ and ‘unknown’ are top types.
//
// ‘string | number’ means we have a set including all numbers
// and all strings. ‘string & number’ would be an empty
// intersection set because string and number are types that
// do not share any common values.
//
//
// This is also where the term narrowing down comes from.  We
// want to have a narrower set of values. If our type is
// ‘any’, we can do a ‘typeof’ check to narrow down to a
// specific set in the type space. We move from a top type
// down to a narrower set of values.
//
// NOTE: There are other checks we can make use of to narrow
// down types.
//

//
// Object Sets
// ===========
//

type Name = {
  name: string;
};

type Age = {
  age: number;
};

type Jedi = Name & Age;

const aayla = {
  name: "Aayla Secura",
  planet: "Ryloth",
};

//
// ‘aayla’ is a valid value for ‘Name’ (extra properties are
// ignored because this is “indirect assignment”.
//
const a1: Name = aayla;

const yoda = {
  name: "Yoda",
  age: 900,
};

//
// ‘yoda’ is a valid value for ‘Age’, ‘Name’, and ‘Person’.
//
const j1: Name = yoda;
const j2: Age = yoda;
const j3: Jedi = yoda;

//
// NOTE: Remember about excess properties during direct vs
// indirect assignment.
//
const a2: Name = {
  name: "Ahsoka Tano",
  age: 27,
};

export const NAME = "e04 object types";

//
// A type is a classification of data that defines the
// operations that can be performed on that data, the meaning
// of the data, and the set of allowed values.
//

const log: Console["log"] = console.log.bind(console);

type Name = {
  name: string;
};

type Age = {
  age: number;
};

type Person = Name & Age;

const person: Person = {
  name: "Ada",
  age: 42,
};

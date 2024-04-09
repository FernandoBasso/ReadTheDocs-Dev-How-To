export const MODNAME = "e12a-form-recursive";

////
// A type description of a form with a few fields, some of
// which are nested.
//
type Form = {
  first: string;
  last: string;
  address: {
    street: {
      name: string;
      num: number;
    };
  };
};

////
// A type that can check whether the path to any of the form
// field matches.
// 
type Path<T> = T extends object
  ? { [Key in keyof T]: [Key] | [Key, ...Path<T[Key]>]}[keyof T]
  : never;

////
// Create the path with Path<Form> then assign an array describing
// the path to it.
//
// If any part of the path doesn't match, then the it doesn't
// type-check.
//
const streetNameCtrl: Path<Form> = ["address", "street", "name"];
const wrong: Path<Form> = ["address", "stret", "name"];

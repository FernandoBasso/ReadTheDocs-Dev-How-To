type Person = {
  id: number;
  name: string;
};

type Student = Person & {
  semester: number;
};

//
// Export the types.
//
export { Person, Student };

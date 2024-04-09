type Person = {
  id: number;
  name: string;
};

type Student = Person & {
  semester: number;
};

const log: Console["log"] = console.log.bind(console);

function print(student: Student): void {
  log(student.name, student.semester);
}

// \@ts-expect-error
// print({ firstName: "Ahsoka" });





// @ts-expect-error
print({
  id: 1,
  name: "Ahsoka",

});

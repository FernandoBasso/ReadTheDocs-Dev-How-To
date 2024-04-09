export {};

const log = console.log.bind(console);

function createJedi() {
  return {
    id: 1,
    name: "Ahsoka Tano",
  };
}

type Jedi = {
  id: number;
  name: string;
};

function printJedi(jedi: Jedi): void {
  log(jedi.id, jedi.name);
}

const ahsoka: Jedi = { id: 1, name: "Ahsoka Tano" };

printJedi(ahsoka);



printJedi({ id: "2", name: "Aayla Secura" });


// const ahsoka: Jedi = createJedi();

function getName(jedi: { name: string }): string {
  return person.name;
}

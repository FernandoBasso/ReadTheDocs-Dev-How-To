export {};

const log = console.log.bind(console);

type Jedi = {
  id: number;
  name: string;
  level: number;
};

type Padawan = {
  id: number;
  name: string;
};

function printPadawan(padawan: Padawan): void {
  log(padawan.id, padawan.name);
}

const aayla: Padawan = {
  id: 1,
  name: "Aayla Secura",
  level: 97,
};

printPadawan(aayla);

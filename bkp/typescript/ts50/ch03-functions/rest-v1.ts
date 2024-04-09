export const NAME = "rest v1";

const log: Console["log"] = console.log.bind(console);

function toUpper(s: string): string {
  return s.toUpperCase();
}

//
// Note the rest parameter type. The ‘...tags’ with the
// type ‘string[]’ type.
//
function search(term: string, ...tags: string[]): void {
  log(tags.map(toUpper));
}

//
// All valid invocations of ‘search’. We pass tags as
// individual parameters.
//
search("TypeScript");
search("TypeScript", "tutorial");
search("TypeScript", "guide", "tutorial", "functions");

//
// Invalid. The tags have to be passed one by one, not
// as an array of tags.
//
// search("TypeScript", ["guide", "tutorial"]);
//

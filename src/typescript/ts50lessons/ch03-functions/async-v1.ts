export const NAME = "async v1";

const log: Console["log"] = console.log.bind(console);

type Result = {
  title: string;
  url: string;
  abstract: string;
};

async function search(
  query: string,
  tags?: string[],
): Promise<Result[]> {
  let queryString = `?query=${query}`;

  if (tags && tags.length) {
    queryString += `&tags=${tags.join("")}`;
  }

  const response = await fetch(`/search${queryString}`);

  const results = await response.json();

  //
  // The return type becomes ‘Promise<Result[]>’.
  //
  return results as Result[];
}

//
// As long we are in a module, TypeScript supports top-level
// async functions.
//

//
// We do not write ‘declare async function f() ...’, but
// because of the return annotation...
//
declare function f(num: number): Promise<number>;

//
// ...‘numResult’ is of the type ‘Promise<number>’.
//
const numResult = f(1);


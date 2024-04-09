export const NAME = "substitutability void v3";

const log: Console["log"] = console.log.bind(console);

type Result = {
  title: string;
  url: string;
  abstract: string;
};

type SearchFn = (
  query: string,
  tags?: string[] | undefined,
) => Promise<Result[]>;

function search(
  query: string,
  callback: (results: Result[]) => void, // <1>
  tags?: string[],
) {
  let queryString = `?query=${query}`;

  if (tags && tags.length) {
    queryString += `&tags=${tags.join(",")}`;
  }

  fetch(`/search${queryString}`)
    .then(res => res.json() as Promise<Result[]>)
    .then(results => {
      //
      // 1. Suppose we know we intend to pass a callback
      // that returns a number.
      //
      const value = callback(results);

      //
      // 3. The signature of our callback says it returns the
      // type ‘void’ (which means the return value will be
      // considered to always be ‘undefined’). Even thought
      // we know we passed a callback that returns a number,
      // the types do not match. We can't do ‘undefined + 1’.
      //
      const increased = value + 1;
    }
}

//
// 1. Note the callback returns ‘void’.
//

//
// Our ‘log’ function return the type ‘void’.
//
search("Yoda", log);

function getResultsLength(results: Result[]): number {
  return results.length;
}

//
// 2. ‘getResultsLength’ returns a number. 
//
search("Aayla", getResultsLength);

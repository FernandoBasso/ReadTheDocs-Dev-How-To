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
    .then(callback);

    // Wy do they do this useless use of callback function award thing?
    // .then(results => callback(results));
}

//
// 1. Note the callback returns ‘void’.
//

//
// Our ‘log’ function return the type ‘void’.
//
search("Yoda", log);

function searchHandler(results: Result[]): number {
  return results.length;
}

//
// But we can also pass a callback that returns any
// non-void type. ‘searchHandler’ returns a ‘number’.
//
// Type-wise, the return value is handled as ‘undefined’.
// It is there as a number at runtime, but on the type
// level (at type context), it is considered ‘void’.
//
search("Aayla", searchHandler);

export const NAME = "overload v2";

const log: Console["log"] = console.log.bind(console);

type Result = {
  title: string;
  url: string;
  abstract: string;
};

//
// It is possible to extract the type of the overloaded search into
// a type alias and use it to annotate the variable used to store
// the search arrow function implementation.
//

type SearchOverloadFn = {
  (
    term: string,
    tags?: string[] | undefined,
  ): Promise<Result[]> | void;
  (
    term: string,
    callback: (results: Result[]) => void,
    tags?: string[] | undefined,
  ): void;
};

const searchWithOverloads: SearchOverloadFn = (
  term: string,
  p2?: string[] | ((results: Result[]) => void),
  p3?: string[],
) => { // Promise<Result[]> | void => {
  //
  // We only have a callback if ‘p2’ is a function.
  //
  const callback = typeof p2 === "function" ? p2 : undefined;

  //
  // We have ‘tags’ if ‘p2’ is defined and an array, or if
  // ‘p3’ is defined and an array.
  //
  const tags =
    typeof p2 !== "undefined" && Array.isArray(p2) ? p2 :
    typeof p3 !== "undefined" && Array.isArray(p3) ? p3 :
    undefined;

  let queryString = `?query=${term}`;

  if (tags && tags.length) {
    //
    // ‘tags’ at this point can only possibly be an array.
    //
    queryString += `&tags=${tags.join()}`;
  }

  //
  // The actual fetching of results!
  //
  const results: Promise<Result[]> =
  fetch(`/search${queryString}`).then((response) =>
    response.json()
  );

  //
  // ‘callback’ is either ‘undefined’ or a function, as
  // seen above.
  //
  if (callback) {
    //
    // Now it's definitely a function! So let's then() the
    // results and call the callback! We don't return
    // anything. This is equivalent to ‘void’.
    //
    results.then((res) => callback(res));

    return;
  } else {
    //
    // Otherwise, we have to return a promise with results
    // as described in the first function overload.
    //
    return results;
  }
};

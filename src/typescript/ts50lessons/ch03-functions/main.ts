export const NAME = "main";

const log: Console["log"] = console.log.bind(console);

type Result = {
  title: string;
  url: string;
  abstract: string;
};

/**
 * Sends search query to backend to get filtered results.
 *
 * @param query The query search string.
 * @param tags The tags we want to include in the filtering.
 * @return The filtered result.
 */
async function search(
  query: string,
  tags?: string[],
): Promise<Result[]> {
  let queryString = `?query=${query}`;

  if (tags && tags.length) {
    queryString += `&tags=${tags.join(",")})`
  }

  return fetch(`/search${queryString}`)
    .then(response => response.json());
}

type AssembleFn = (includeTags: boolean) => string;

type Query = {
  query: string;
  tags?: string[];
  assemble: AssembleFn;
};

type SearchFn = (
  query: string,
  tags?: string[] | undefined,
) => Promise<Result[]>;

const query: Query = {
  query: "Ember",
  tags: ["javascript"],
  assemble(includeTags = false) {
    let query = `?query=${this.query}`;

    if (includeTags && typeof this.tags !== "undefined") {
      query += `&${this.tags.join(",")}`;
    }

    return query;
  }
}

declare function displaySearch(
  inputId: string,
  outputId: string,
  search: SearchFn,
): void;

displaySearch(
  "searchField",
  "result",
  function(query, _tags) {
    return Promise.resolve([{
      title: `The ${query} test book`,
      url: `/${query}-design-patterns`,
      abstract: `A practical book on ${query}`,
    }]);
  },
);

const testSearch: SearchFn = function(query, _tags) {
  // All types still intact
  return Promise.resolve([{
    title: `The ${query} test book`,
    url: `/${query}-design-patterns`,
    abstract: `A practical book on ${query}`
  }]);
};

//
// Because we are not using any parameters inside the
// callback, the type checker doesn't bother us saying
// the first parameter is not optional. It required
// if we actually use it. Otherwise, it just does some
// inference and sees we are not using it, and thinks
// we know what we are doing.
//
const dummyContentSearchFn: SearchFn = function() {
  return Promise.resolve([{
    title: "Form Design Patterns",
    url: "/form-design-patterns",
    abstract: "A practical book on accessible forms",
  }]);
};

dummyContentSearchFn();

/*
TypeScript calls this behavior substitutability. We can substitute
one function signature for another if it makes sense. Leaving out
parameters if we don’t have any use for them inside the function
body is OK. The code will still work. This is one of the many ways
TypeScript is less strict and more pragmatic, to conform to the way
JavaScript works.
*/


// vim: set textwidth=68:

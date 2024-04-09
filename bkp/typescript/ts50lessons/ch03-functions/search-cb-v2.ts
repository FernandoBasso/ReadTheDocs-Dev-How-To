export const NAME = "search callback v2";

type SearchFn = (
  query: string,
  tags?: string[] | undefined,
) => Promise<Result[]>;

declare function displaySearch(
  inputId: string,
  outputId: string,
  search: SearchFn,
): void;

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

//
// ‘search’ matches the type of the callback we need
// for ‘displaySearch’.
//
displaySearch("search-field", "output", search);

/*
Unlinke object types/shapes, function types/shapes only
care about the position and structure of parameters, but
not about their names. We don't need to match the type
‘query’ and ‘tags’ names. We can implement the function
with ‘term’ and ‘opts’ and we are fine.
*/

const testSearch: SearchFn = function (term, _opts) {
  return Promise.resolve([
    {
      title: `The ${term} test book`,
      url: `/${term}-design-patterns`,
      abstract: `A practical book on ${term}`,
    },
  ]);
};

//
// Or we could create a search function on the fly.
//
displaySearch("searchField", "result", testSearch);

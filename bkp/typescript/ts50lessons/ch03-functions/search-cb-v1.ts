export const NAME = "search callback v1";

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

//
// Or we could create a search function on the fly.
//
//                                           <1>
displaySearch("searchField", "result", function (query, _tags) {
  return Promise.resolve([
    {
      title: `The ${query} test book`,
      url: `/${query}-design-patterns`,
      abstract: `A practical book on ${query}`,
    },
  ]);
});

//
// 1. Note we don't type query and _tags. They are correctly
// inferred to the type SearchFn.
//

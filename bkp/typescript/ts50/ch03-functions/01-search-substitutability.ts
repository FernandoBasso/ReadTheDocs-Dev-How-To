export const NAME = "substitutability";

// A helper type with the results we expect
// from calling the back end
type Result = {
  title: string;
  url: string;
  abstract: string;
};

function search(query: string, tags?: string[]): Promise<Result[]> {
  let queryString = `?query=${query}`;

  if (tags && tags.length) {
    queryString += `&tags=${tags.join()}`;
  }
  return fetch(`/search${queryString}`).then((response) => response.json());
}

type SearchFn = typeof search;

type AssembleFn = (includeTags: boolean) => string;

type Query = {
  query: string;
  tags?: string[];
  assemble: AssembleFn;
};

// This is a valid search Function
const dummyContentSearchFn: SearchFn = function () {
  return Promise.resolve([
    {
      title: `Form Design Patterns`,
      url: `/form-design-patterns`,
      abstract: `A practical book on accessible forms`,
    },
  ]);
};

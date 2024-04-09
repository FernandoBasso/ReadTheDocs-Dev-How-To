export const NAME = "substitutability v1";

type Result = {
  title: string;
  url: string;
  abstract: string;
};

type SearchFn = (
  query: string,
  tags?: string[] | undefined,
) => Promise<Result[]>;

//
// A function definition does not need to specify parameters
// in the signature (even if the type requires them) if the
// function does not use the parameters in the body.
//
const dummySearch: SearchFn = function dummySearch () {
  return Promise.resolve([
    {
      title: "The Force",
      url: "https://fernandobasso.dev",
      abstract: "Learn the ways of the force.",
    },
  ]);
};

//
// Still, we can't call it without providing the parameters
// the type mandates.
//
dummySearch();

//
// ‘query’ is the required parameter. We are OK here.
//
dummySearch("jedi");

declare function displaySearch(
  inputId: string,
  outputId: string,
  search: SearchFn,
): void;

//
// But it works as a callback function! 😲
//
displaySearch("input", "output", dummySearch);

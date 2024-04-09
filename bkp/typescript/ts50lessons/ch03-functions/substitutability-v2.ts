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
// Here the contract is not created with a type annotation,
// but with the function signature, which specifies no arguments.
// The return type is also inferred.
//
function dummySearch() {
  return Promise.resolve([
    {
      title: "The Force",
      url: "https://fernandobasso.dev",
      abstract: "Learn the ways of the force.",
    },
  ]);
};

//
// Now this is the correct call.
//
dummySearch();

//
// And this is wrong, because the contract (function
// signature in this case) says not arguments are
// to be passed.
//
dummySearch("jedi");


declare function displaySearch(
  inputId: string,
  outputId: string,
  search: SearchFn,
): void;

//
//
// As seen above, it does not work if directly called with
// non-type-matching arguments. But it works as a callback
// function! 😲
//
displaySearch("input", "output", dummySearch);

//
// Substitutability works because the return type remains the same.
//

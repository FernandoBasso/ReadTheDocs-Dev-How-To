export const NAME = "generator functions v2";

const log: Console["log"] = console.log.bind(console);

type Result = {
  url: string;
  title: string;
  intro: string;
};

type PollingResults = {
  results: Result[];
  done: boolean;
};

//
// This is not actually polling. It will be used
// to poll by some other client code.
//
async function polling(
  term: string,
): Promise<PollingResults> {
  return fetch(`/pollingSearch?query=${term}`)
    .then(res => res.json());
}

function append(result: Result) {
  const node = document.createElement('li');
  node.innerHTML = `<a href="${result.url}">${result.title}</a>`;
  document.querySelector('#results')?.append(node);
}

//
// This is the return type of this generator function:
//
//   AsyncGenerator<
//     Result[], // We yield Result[].
//     void,     // We return nothing.
//     unknown   // We don't pass in anything.
//   >;
//

type GetResultsGenFn = AsyncGenerator<Result[], void, unknown>;

//
// The generator function uses ‘polling()’ and keeps yielding
// results until the server stops returning results.
//
async function* getResults(term: string): GetResultsGenFn {
  let state: PollingResults | undefined = undefined;

  //
  // This ‘do/while’ is what actually does the polling.
  //
  do {
    state = await polling(term);
    yield state.results;
  } while (!state.done);
}

async function handleChange(this: HTMLElement, _ev: Event): Promise<void> {
  if (!(this instanceof HTMLInputElement)) return;

  let resultsGen: GetResultsGenFn = getResults(this.value);

  let next: IteratorResult<Result[], void>;

  do {
    next = await resultsGen.next();

    if (typeof next.value === "undefined") break;

    //
    // Uses ‘append()’ defined above. Map over each element
    // of ‘Result[]’ and append it as as ‘<li><a> ...’ on the DOM.
    //
    next.value.map(append);
  } while (!next.done);
}

document.getElementById("search-field")
  ?.addEventListener("change", handleChange);

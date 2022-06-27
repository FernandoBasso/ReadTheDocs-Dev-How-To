export const NAME = "this that v2";

const log: Console["log"] = console.log.bind(console);

type Result = {
  title: string;
  url: string;
  abstract: string;
};

type SearchFn = (
  query: string,
  tags?: string[] | undefined
) => Promise<Result[]>;

/*
<form action="/search" method="POST">
  <label for="search">Search the site</label>
  <input type="search" id="search">
  <button type="submit">
</form>
<div id="output" hidden></div>
*/


//
// ‘this’ is now not bounded.
//
// In JavaScript, ‘this’ is NOT static (like in most other
// languages). The value of ‘this’ is dynamic and depends
// on how functions and methods are invoked rather then on
// how they are defined.
//
// Here, TypeScript is not sure we are going to call the
// function in a way that makes ‘this’ have the correct
// value. Thus, the type errors and warnings.
//
function handleChangeBroken() {
  this.parentElement?.classList.add("active");

  //
  // A type guard to assert it is an ‘HTMLInputElement’,
  // which we are sure has a ‘value’ property.
  //
  if (!(this instanceof HTMLInputElement)) return;

  //
  // If not an ‘HTMLInputElement’, we return above and do
  // not get to this point. But if we get here, we have
  // an ‘HTMLInputElement’ which certainly has a ‘value’
  // property.
  //
  const term = this.value;

  search(term).then(log);
}

//
// Note that here we more or less do like in Python, where
// we pass the `self` parameter if we want to have the
// “this context” available.
//
// After code is transpiled, this parameter goes away.
//
function handleChangeFixed(this: HTMLElement, search: SearchFn): void {
  this.parentElement?.classList.add("active");

  //
  // A type guard to assert it is an ‘HTMLInputElement’,
  // which we are sure has a ‘value’ property.
  //
  if (!(this instanceof HTMLInputElement)) return;

  //
  // If not an ‘HTMLInputElement’, we return above and do
  // not get to this point. But if we get here, we have
  // an ‘HTMLInputElement’ which certainly has a ‘value’
  // property.
  //
  const term = this.value;

  search(term).then(log);
}

//
// We can only call this function when we are sure ‘this’
// will be a subtype of ‘HTMLElement’. It also requires
// a ‘this’ context.
//
handleChangeFixed();
//
// The 'this' context of type 'void' is not assignable to method's
// 'this' of type 'HTMLElement'.ts(2684)
// 

function displaySearch(
  inputId: "string",
  outputId: "string",
  search: SearchFn
): void {
  document
    .getElementById("#search")
    ?.addEventListener("change", handleChangeFixed);
    //
    // We still need to fix this.
    //
}

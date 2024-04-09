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

function displaySearch(
  inputId: "string",
  outputId: "string",
  search: SearchFn
): void {
  document
    .getElementById("#search")
    ?.addEventListener("change", function handleChange() {
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
    });
}

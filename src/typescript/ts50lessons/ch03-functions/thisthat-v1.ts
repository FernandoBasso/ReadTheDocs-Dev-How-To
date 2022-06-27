export const NAME = "this that v1";

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

      const searchTerm = this.value;
    });
}

//
// Compare with the next example, because:
//
// • ‘getElementById()’ returns  ‘HTMLElement | null’.
// • ‘querySelector()’ returns ‘Element | null’.
//

function displaySearchV2(
  inputId: "string",
  outputId: "string",
  search: SearchFn
): void {
  document
    .querySelector("#search")
    ?.addEventListener("change", function handleChange() {
      this.parentElement?.classList.add("active");

      const searchTerm = this.value;
    });
}

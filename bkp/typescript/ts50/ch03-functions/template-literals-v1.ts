export const NAME = "template literals v1";

//
// Template Literals and Template Tags
//

const log: Console["log"] = console.log.bind(console);

const term = "Ember";
const results = 12;

const text = `You searched for ‘${term}’ and got ‘${results}’ results.`;

log(text);
// → You searched for ‘Ember’ and got ‘12’ results.

const result = {
  title: "A guide to @@starthl@@Ember@@endhl@@.js",
  url: "/a-guide-to-ember",
  description: "The framework @@starthl@@Ember@@endhl@@.js in a nutshell",
};

function highlightV1(strings: TemplateStringsArray, ...values: string[]) {
  let str = "";

  strings.forEach((templ, idx) => {
    let expr =
      values[idx]
        ?.replace("@@starthl@@", "<mark>")
        ?.replace("@@endhl@@", "</mark>") ?? "";
    str += templ + expr;
  });

  return str;
}

//
// My version without all the reassignments to str. Using `reduce()`
// instead of `forEach()` because I prefer the more functional style.
// Also using a regexp with `replace()` to make global matches.
//
function highlight(
  strings: TemplateStringsArray,
  ...values: string[]
): string {
  return strings.reduce((acc, templ, idx) => {
    let expr =
      values[idx]
        ?.replace(/@@starthl@@/g, "<mark>")
        ?.replace(/@@endhl@@/g, "</mark>") ?? "";

    return (acc += templ + expr);
  }, "");
}

let markup = highlight`<li>${result.title}</li>`;
log(markup);

const force =
  "May the @@starthl@@force@@endhl@@ be with @@starthl@@you@@endhl@@.";
let hello = highlight`<p>${force}</p>`;
log(hello);

function createResultTemplate(results: { title: string }[]): string {
  //
  // Remember that map returns an array. When an array is printed as
  // a string, it has to first be joined as a string, and `join()`
  // implicitly joins elements of arrays separating them with a
  // comma.
  //
  // We use an explicit `join('')` below to avoid our `<li>` be
  // intermingled with commas.
  //
  // • https://stackoverflow.com/questions/45812160/unexpected-comma-using-map
  //
  return `<ul>
    ${results.map((result) => {
      return highlight`<li>${result.title}</li>`;
    }).join('')};
</ul>`;
}

log(
  createResultTemplate([
    { title: "The Force" },
    { title: "Making your own lightsaber" },
  ])
);

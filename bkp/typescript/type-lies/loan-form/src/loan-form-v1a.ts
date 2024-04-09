export {};

const log: Console["log"] = console.log.bind(console);

log("v1a");

function getInputValue(selector: string): string {
  const input = document.querySelector(selector);
  const value = input.value;
  return value;
}

(function init() {
  //
  // All seems fine with this!
  //
  const form = document.querySelector("form");
  const button = document.querySelector("button");

  form.addEventListener("submit", function handleSubmit(evt: SubmitEvent) {
    return evt.preventDefault();
  }, false);

  button.addEventListener("click", function handleClick() {
    log(getInputValue("#amount"));
  }, false);
})();

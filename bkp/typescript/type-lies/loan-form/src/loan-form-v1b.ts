export {};

const log: Console["log"] = console.log.bind(console);

log("v1b");

function getInputValue(selector: string): string {
  //
  // But as soon as we add explicit type annotations, then were are
  // politely informed that maybe we have some problems lurking around.
  //
  const input: HTMLInputElement = document.querySelector(selector);
  const value = input.value;
  return value;
}

(function init() {
  const form: HTMLFormElement = document.querySelector("form");
  const button: HTMLButtonElement = document.querySelector("button");

  form.addEventListener("submit", function handleSubmit(evt: SubmitEvent) {
    return evt.preventDefault();
  }, false);

  button.addEventListener("click", function handleClick() {
    log(getInputValue("#amount"));
  }, false);
})();

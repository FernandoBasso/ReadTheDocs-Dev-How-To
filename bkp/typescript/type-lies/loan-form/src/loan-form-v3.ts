export {};

const log: Console["log"] = console.log.bind(console);

log("v3");

function getInputValue(selector: string): string {
  //
  // Non-null assertion can be replaced with `as` assertions for this
  // particular case.
  //
  // Also fixes the problem, right? RIGHT?!!!
  //      --+--
  //         \
  //          +--------------------------------------------------+
  //                                                              \
  //                                                               v
  const input = document.querySelector<HTMLInputElement>(selector) as HTMLInputElement;
  const value = input.value;
  return value;
}

(function init() {
  const form: HTMLFormElement = document.querySelector("form") as HTMLFormElement;
  const button: HTMLButtonElement = document.querySelector("button") as HTMLButtonElement;

  form.addEventListener("submit", function handleSubmit(evt: SubmitEvent) {
    return evt.preventDefault();
  }, false);

  button.addEventListener("click", function handleClick() {
    log(getInputValue("#amount"));
  }, false);
})();

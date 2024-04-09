export {};

const log: Console["log"] = console.log.bind(console);

log("v2");

function getInputValue(selector: string): string {
  //
  // OK. We fixed the type problems, right? RIGHT?!!
  //        --+--
  //           \
  //            +-----------------------------------------------+
  //                                                             \
  //                                                              ↓
  const input = document.querySelector<HTMLInputElement>(selector)!;
  const value = input.value;
  return value;
}

(function init() {
  const form: HTMLFormElement = document.querySelector("form")!;
  const button: HTMLButtonElement = document.querySelector("button")!;

  form.addEventListener("submit", function handleSubmit(evt: SubmitEvent) {
    return evt.preventDefault();
  }, false);

  button.addEventListener("click", function handleClick() {
    log(getInputValue("#amount"));
  }, false);
})();

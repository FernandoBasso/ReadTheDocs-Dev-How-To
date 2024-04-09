const l = console.log.bind(console);

function delay(timeout, callback) {
  setTimeout(function timeoutFn() {
    callback(`Slept for ${timeout} millis`);
  }, timeout);
}

delay(1500, (msg) => {
  l(msg);
});


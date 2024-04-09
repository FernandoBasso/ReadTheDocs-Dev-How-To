function* hello(param) {
  let count = 0;
  while (true) {
    yield count;
    count += 1;
  }
}

let generator = hello('first call');

let result = generator.next();

function logResult() {
  console.log('result', result);
  result = generator.next();
  setTimeout(logResult, 500);
}

logResult();


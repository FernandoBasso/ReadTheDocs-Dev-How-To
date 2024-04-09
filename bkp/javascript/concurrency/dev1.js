//
// Coordinate async using promise chains.
//

const l = console.log.bind(console);

function fakeAjax(url, cb) {
  var fake_responses = {
    file1: 'The Mighty First File',
    file2: 'The middle file!',
    file3: 'The humble file three, at your service.',
  };

  var randomDelay = (Math.round(Math.random() * 1e4) % 1e8) + 1000;

  setTimeout(function () {
    cb(fake_responses[url]);
  }, randomDelay);
}

function output(text) {
  l(text);
}

function getFile(file) {
  return new Promise(function (resolve) {
    fakeAjax(file, resolve);
  });
}

var p1 = getFile('file1');
var p2 = getFile('file2');
var p3 = getFile('file3');

p1.then(output)
  .then(function () { return p2; })
  .then(output)
  .then(function () { return p3; })
  .then(output)
  .then(function () { output('Complete!'); });


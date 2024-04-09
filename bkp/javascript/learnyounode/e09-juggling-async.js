'use strict';

const http = require('http');

/**
 * Alias to ‘console.log’.
 *
 * @param {any...}
 */
const l = console.log.bind(console);

/**
 * Helper print function that only takes one param.
 *
 * Useful as a print/log callback in situations where the callback
 * would be passed several parameters (such as ‘forEach’) but we
 * need to make sure only one param is taken.
 *
 * @param {any} arg
 */
const print1 = function print1 (arg) {
  l(arg);
};

const url1 = process.argv[2];
const url2 = process.argv[3];
const url3 = process.argv[4];

const results = [];

/**
 * Makes an HTTP get request and invokes cb with the data.
 *
 * @param {string} url
 * @param {function(string)} cb
 */
function collect(url, cb) {
  http.get(url, function (resp) {
    resp.setEncoding('utf8');
    let acc = '';

    resp.on('end', function (data) {
      cb(acc || null);
    });

    resp.on('data', function (data) {
      acc += data;
    });
  });
}

const urls = [url1, url2, url3];
let count = 0;

urls.forEach(function (url, i) {
  collect(url, function (data) {
    results[i] = data;
    count += 1;
    if (count === 3) {
      results.forEach(print1);
    }
  });
});


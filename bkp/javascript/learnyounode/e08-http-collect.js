'use strict';

const http = require('http');

const l = console.log.bind(console);

const url = process.argv[2];

http.get(url, function (resp) {
  resp.setEncoding('utf8');

  let acc = '';

  resp.on('end', function (data) {
    l(acc.length);
    l(acc);
  });

  resp.on('data', function (data) {
    acc += data;
  });
});


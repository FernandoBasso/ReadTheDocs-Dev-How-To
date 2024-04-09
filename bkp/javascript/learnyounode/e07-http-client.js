'use strict';

const { get } = require('http');

const l = console.log.bind(console);

const url = process.argv[2];

get(url, function (response) {
  response.setEncoding('utf8');
  response.on('data', l);
});


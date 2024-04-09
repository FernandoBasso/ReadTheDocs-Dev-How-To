'use strict';

const http = require('http');

const port = +process.argv[2];

const l = console.log.bind(console);

const serv = http.createServer(function httpHandler (req, res) {
  if (req.method !== 'POST') {
    res.end('Only POST requests will be accepted.\n');
    return null;
  }

  req.on('end', function handleChunk (chunk) {
    res.write(data.toString().toUpperCase());
  });

  req.on('data', function handleChunk (data) {
    res.write(data.toString().toUpperCase());
  });

});

serv.listen(port);


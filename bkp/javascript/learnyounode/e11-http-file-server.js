'use strict';

const http = require('http');
const fs = require('fs');

const port = +process.argv[2];
const path = process.argv[3];

const serv = http.createServer(function (req, res) {
  res.writeHead(200, { 'Content-Type': 'text/plain' });

  const stream = fs.createReadStream(path);

  stream.on('open', function () {
    stream.pipe(res);
  });
});

serv.listen(port);


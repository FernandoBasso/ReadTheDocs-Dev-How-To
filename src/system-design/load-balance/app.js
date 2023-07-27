const { createServer } = require('node:http');
const { networkInterfaces, hostname } = require('node:os');

const log = console.log.bind(console);

const server = createServer(function handler(req, res) {
  const host = req.headers.host
  const { eth0 } = networkInterfaces();

  log('==== Responding to request:', {
    host,
    hostname: hostname(),
    eth0,
  });

  res.end(`HOSTNAME: ${hostname()}, IP: ${eth0[0].address}`);
});

server.listen(8181, () => {
  log('==== Server started on 8181');
});

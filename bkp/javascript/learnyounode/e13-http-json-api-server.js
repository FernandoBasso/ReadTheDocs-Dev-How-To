'use strict';

const http = require('http');

//
// /api/parsetime?iso=2013-08-10T12:10:15.474Z
//
// {
//   "hour": 14,
//   "minute": 23,
//   "second": 15
// }
//
//
// /api/unixtime
//
// { "unixtime": 1376136615474 }
//

// date.toISOString()
// Date.getTime()

const port = process.argv[2];

const l = console.log.bind(console);

/**
 * Merge key/val arrays into key/value object.
 *
 * @param {array} keys
 * @param {array} vals
 */
const merge = (keys, vals) => {
  return keys.reduce((acc, key, idx) => {
    return {...acc, [key]: vals[idx]};
  }, {});
};

const getTime = function getTime (isoString) {
  return {
    unixtime: new Date(isoString).getTime(),
  };
};

const hmsDateJson = function hmsDateJson (isoString) {
  const d = new Date(isoString)
  const h = d.getHours();
  const m = d.getMinutes();
  const s = d.getSeconds();

  return merge(['hour', 'minute', 'second'], [h, m, s]);
};

const parseQuery = (q) => {
  return q.replace(/^\?/, '',).split(/&/g)
    .filter(p => p)
    .reduce((acc, param) => {
      const [k, v] = param.split('=');
      return {...acc, [k]: v};
    }, {});
};

const serv = http.createServer(function servHandler (req, res) {
  res.writeHead(200, { 'Content-Type': 'application/json' });

  const { pathname, search } = new URL(req.url, 'http://localhost:3000');

  const { iso } = parseQuery(search);

  switch (pathname) {
    case '/api/parsetime':
      res.end(JSON.stringify(hmsDateJson(iso)) + '\n');
      break;
    case '/api/unixtime':
      res.end(JSON.stringify(getTime(iso)) + '\n');
      break;
    default:
      res.end('We cannot handle that route.\n');
  }
});

serv.listen(port);


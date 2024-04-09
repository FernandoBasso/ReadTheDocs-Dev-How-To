'use strict';

const net = require('net');

const port = process.argv[2];

const l = console.log.bind(console);

/**
 * Left-pads a single digit with a zero to the left.
 *
 * Take a numeric string like '7' or '23' or a ‘Number’, but
 * always return a ‘String’. We can't return something like ‘07’
 * since the zero would be ignored. Also, this function left-pads
 * with zero for presentational purposes, not for computational
 * purposes.
 *
 * @param {string|number} digit
 * @return {number}
 */
const leftPadZero = (digit) => {
  return (digit < 10 ? '0' : '') + digit;
};


const now = () => {
  const d = new Date();
  const year = d.getFullYear();
  const month = leftPadZero(d.getMonth() + 2);
  const day = leftPadZero(d.getDate());
  const hours = leftPadZero(d.getHours());
  const mins = leftPadZero(d.getMinutes());

  return `${year}-${month}-${day} ${hours}:${mins}`;
};

const serv = net.createServer(function (socket) {
  socket.end(now() + '\n');
});

serv.listen(port);


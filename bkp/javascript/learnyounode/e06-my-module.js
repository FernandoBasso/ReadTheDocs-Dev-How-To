'use strict';

// Can't use ‘{ readdir }’ or tests don't run because of the way
// ‘fs’ is mocked.
const fs = require('fs');
const { extname } = require('path');

module.exports = function (dir, ext, cb) {
  fs.readdir(dir, function (err, list) {
    if (err) return cb(err);

    const names = list.reduce((acc, s) => {
      if (extname(s) !== `.${ext}`) return acc;
      return [...acc, s];
    }, []);

    return cb(null, names);
  });
};


/// <reference path="../lib/typedefs.js" />

import { readFileSync } from 'fs';

import {
  log,
  Left,
  Right,
} from '../lib/index.js';

/**
 * Turns a try/catch into an `Either`, composable container.
 *
 * @param {Function} f
 * @return {Either}
 */
const tryCatch = f => {
  try {
    return Right(f());
  } catch (err) {
    return Left(err);
  }
};

/**
 * Gets the port from the config file.
 *
 * Returns the port from the config file or 3000 if impossible to get
 * port from config file.
 *
 * @param {string} configPath The path the config file.
 * @return {number}
 *
 * @example
 * getPort(); // No path, or incorrect path.
 * // → 3000
 *
 * But if we have a proper config file, with a proper path param, then
 * we get back that port.
 *
 *   $ cat ./configs/config.json
 *   {
 *     "port": 8888
 *   }
 *
 * @example
 * getPort('./configs/config.json');
 * // → 8888
 */
const getPort = (configPath) => {
  return tryCatch(() => readFileSync(configPath))
    .chain(jsonCfg => tryCatch(() => JSON.parse(jsonCfg)))
    .fold(_ => 3000, objCfg => objCfg.port);
};

log(getPort());
// → 3000

log(getPort('./vid04/wrong-path.json'));
// → 3000

log(String(getPort('./vid04/config-04c-invalid.json')));
// → 8888

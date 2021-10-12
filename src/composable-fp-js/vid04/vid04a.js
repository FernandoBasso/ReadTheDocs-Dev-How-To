/// <reference path="./typedefs.js" />

import { readFileSync } from 'fs';

import {
  log,
} from '../lib/index.js';

/**
 * Gets the port from the config file.
 *
 * Returns the port from the config file or 3000 if impossible to get
 * it from config file.
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
  try {
    const str = readFileSync(configPath);
    const config = JSON.parse(str);
    return config.port;
  } catch (err) {
    return 3000;
  }
}

log(getPort());
// → 3000

log(getPort('./vid04/wrong-path.json'));
// → 3000

log(getPort('./vid04/config-04a.json'));
// → 8888

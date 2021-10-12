/// <reference path="../lib/typedefs.js" />

import { readFileSync } from 'fs';

import { tryCatch } from '../lib/index.js';

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

export { getPort };

//
// jest.config.js
//

//
// As of May 12, 2021 both Node and Jest offer experimental support
// for ES6 Modules (ESM), and we are using those features to run
// JavaScript unit tests in this project.
//
// Docs used as reference for this configuration:
//
// • https://fernandobasso.dev/javascript/nodejs-v12-ES6-module-support.html
// • https://jestjs.io/docs/ecmascript-modules
// • https://nodejs.org/api/esm.html#esm_enabling
//
// Note that we added `"type": "module"` to package.json as well.
//

////
// Sync object
//
/** @type {import('@jest/types').Config.InitialOptions} */
export default {
  verbose: true,
  transform: {},
  testPathIgnorePatterns: ['bkp'],
};

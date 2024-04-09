const l = console.log.bind(console);

function isFn(thing) {
  // return thing.constructor === Function;
  l(typeof thing === 'function');
}


l(isFn(null));
// Cannot read property 'constructor' of undefined.



/* vim: set tw=68 ft=text: */


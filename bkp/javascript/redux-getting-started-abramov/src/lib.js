const l = console.log.bind(console, '====');

const testsPassed = (text = 'All Tests Passed!') => {
  const len = text.length + 10;
  console.info(`${'='.repeat(len)}\n==== ${text} ====\n${'='.repeat(len)}`);
}

/**
 * Deep freezes an object.
 *
 * Take from:
 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/freeze
 *
 * @param {object} obj
 */
const deepFreeze = (obj) => {
  // Retrieve the property names defined on object
  var propNames = Object.getOwnPropertyNames(obj);

  // Freeze properties before freezing self.

  for (let name of propNames) {
    let value = obj[name];

    obj[name] = value && typeof value === 'object'
      ? deepFreeze(value)
      : value;
  }

  return Object.freeze(obj);
}

export {
  l,
  testsPassed,
  deepFreeze,
};

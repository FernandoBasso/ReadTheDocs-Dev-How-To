/* eslint-disable no-console */
export const l = console.log.bind(console);

export function allTestsPassed(msg = 'All Tests Passed!') {
  const dashes = '-'.repeat(msg.length + 10);
  l('\t', dashes);
  l('\t', '----', msg.toUpperCase(), '----');
  l('\t', dashes);
}


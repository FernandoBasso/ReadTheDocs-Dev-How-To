import { jest } from '@jest/globals'
import { log } from './log';

//
// Beside spying, we also `mockImplementation()` to prevent the logging
// to actually happen and pollute our unit testing output.
//

describe('when not args are provided', () => {
  beforeEach(() => {
    jest.spyOn(global.console, 'log').mockImplementation();
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  it('should only log the divider with the empty line', () => {
    log();

    const calls = console.log.mock.calls;

    expect(calls[0][0]).toEqual('-'.repeat(32));
    expect(calls[1][0]).toBe(undefined);

    jest.restoreAllMocks();
  });

  it('should log devidier, params, and empty line', () => {
    log('hello', 'world', 2 + 3);

    const calls = console.log.mock.calls;

    expect(calls[0][0]).toEqual('-'.repeat(32));
    expect(calls[1][0]).toEqual('hello');
    expect(calls[2][0]).toEqual('world');
    expect(calls[3][0]).toEqual(5);
    expect(calls[4][0]).toEqual(undefined);
  });
});

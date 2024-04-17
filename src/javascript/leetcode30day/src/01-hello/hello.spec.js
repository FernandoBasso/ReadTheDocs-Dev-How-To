import { createHelloWorld } from './hello';

describe('createHelloWorld()', () => {
  it('returns a function', () => {
    var fn = createHelloWorld();
    expect(fn.constructor).toEqual(Function);
  });

  it('returns a function that returns Hello World string', () => {
    var fn = createHelloWorld();
    expect(fn()).toEqual("Hello World");
    expect(createHelloWorld()()).toEqual("Hello World");
  });
});

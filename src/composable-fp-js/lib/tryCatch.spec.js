import { Left, Right } from './Either.js';
import { tryCatch } from './tryCatch.js';

describe('when we get an exception', () => {
  it('should return a Left container', () => {
    expect(
      String(tryCatch(() => undefined.split('-')))
    ).toEqual(
      // This hard-coded TypeError string bothers me. What other
      // approach could be used?
      String(
        Left(
          "TypeError: Cannot read property 'split' of undefined"
        )
      )
    );
  });
});

describe('when we get a successfull result', () => {
  it('should return a Right container', () => {
    expect(
      String(tryCatch(() => 'hello-world'.split('-')))
    ).toEqual(
      String(Right('hello,world'))
    );
  });
});

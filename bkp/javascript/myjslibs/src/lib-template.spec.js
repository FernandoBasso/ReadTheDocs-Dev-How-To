import {
  inject,
} from './lib-template';

// describe('lib-template.js', () => {
//   describe('inject()', () => {
//     describe('when template contains no placeholders', () => {
//       it('should return unmodified string', () => {
//         expect(inject('Hello World!')).toEqual('Hello World!');
//         expect(
//           inject('The Force!', { someProp: 'must not be used' })
//         ).toEqual('The Force!');
//       });
//     });
//       describe('when template string contains placeholders', () => {
//         it('should inejct a single placeholder value', () => {
//           expect(
//             inject('Hello, {key}!', { key: 'World' })
//           ).toEqual('Hello, World!');
//         });
//       });
//   });
// });
//

describe('lib-template.js', () => {
  describe('inject()', () => {
    it('should not change input if it contains no placeholders', () => {
      expect(inject('Hello World!')).toEqual('Hello World!');
    });
    it('should not inject data if input contains no placeholders', () => {
      expect(
        inject('The Force!', { someProp: 'must not be used' })
      ).toEqual('The Force!');
    });
    it('should inejct a single placeholder value', () => {
      expect(
        inject('Hello, {key}!', { key: 'World' })
      ).toEqual('Hello, World!');
    });
  });
});


const { toUpper } = require('./toUpper-v1');

/**
 * Uppercases all strings in the array.
 *
 * @param {string[]} strs
 * @returns {string[]}
 */
function upperCaseAll(strs) {
  return strs.map(toUpper);
}

module.exports = { upperCaseAll };

// ['hello', 'world'] -> ['HELLO', 'WORLD']
// [] -> []

/*

//
// users = [
//   { id: 1, name: 'Ahsoka Tano' }
//   { id: 2, name: 'Aayla Secura' }
// ]
//
const UserList = ({ users }) => {
  return (
    <ul>
      {
        users.map((user) => {
          return <li key={id}>{user.name}</li>;
        });
      }
    </ul>
  );
};

- map lower to upper
  - ['hello', 'world'] -> ['HELLO', 'WORLD]
- map number to number incremented
  - [1, 2, 3] -> [2, 3, 4]
- map float to integer
  - [1.1, 2.1] -> [1, 2]
- map a list of users to a list of <li> DOM elements.
  - [{ name: 'Ahsoka' }, { name: 'Aayla' }] -> [<li>Ahsoka</li>, <li>Aayla</li>]
  - NOTE: JSX syntax.

*/

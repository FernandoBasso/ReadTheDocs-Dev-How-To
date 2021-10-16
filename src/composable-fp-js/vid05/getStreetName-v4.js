/// <reference path="./getStreetName-typedefs.js" />

import {
  fromNullable,
  id,
} from '../lib/index.js';

/**
 * Gets the street name, if possible.
 *
 * @param {User} user
 * @return {string = 'no street'} The stret name or
 */
function getStreetName(user) {
  return fromNullable(user.address)
    .chain(address => fromNullable(address.street))
    .map(street => street.name)
    .fold(_ => 'no street', id);
}

export { getStreetName };

/// <reference path="./getStreetName-typedefs.js" />

/**
 * Gets the street name, if possible.
 *
 * @param {User} user
 * @return {string = 'no street'} The stret name or
 */
function getStreetName(user) {
  const noStreet = 'no street';

  const { address } = user;
  if (!address) return noStreet;

  const { street } = address;
  if (!street) return noStreet;

  return street.name;
}

export { getStreetName };

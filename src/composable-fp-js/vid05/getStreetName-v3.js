/// <reference path="./getStreetName-typedefs.js" />

/**
 * Gets the street name, if possible.
 *
 * @param {User} user
 * @return {string = 'no street'} The stret name or
 */
function getStreetName(user) {
  return user?.address?.street?.name ?? 'no street';
}

export { getStreetName };

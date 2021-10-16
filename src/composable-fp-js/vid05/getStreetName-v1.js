/// <reference path="./getStreetName-typedefs.js" />

/**
 * Gets the street name, if possible.
 *
 * @param {User} user
 * @return {string = 'no street'} The stret name or
 */
function getStreetName(user) {
  const address = user.address;

  if (address) {
    const street = address.street;

    if (street) {
      return street.name;
    }
  }

  return 'no street';
}

export { getStreetName };

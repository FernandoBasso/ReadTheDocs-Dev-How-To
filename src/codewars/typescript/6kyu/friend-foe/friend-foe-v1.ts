/**
 * Friends have names of 4 (four) letters, like “Yoda”,
 * “Leia” or “Luke" 🤣.
 */
const FRIEND_NAME_LENGHT = 4;

/**
 * Checks whether the given string has length `len`.
 */
const hasLengthOf = function hasLengthOf(len: number) {
  return function ({ length }: string): boolean {
    return length === len;
  }
}

/**
 * Filter names that have length `FRIEND_NAME_LENGHT`.
 */
function filterFriends(names: string[]){
  return names.filter(hasLengthOf(FRIEND_NAME_LENGHT));
}

export { filterFriends };

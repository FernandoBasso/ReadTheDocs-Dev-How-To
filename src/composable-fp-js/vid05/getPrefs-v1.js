/// <reference path="./getPrefs-typedefs.js" />

import defaultPrefs from './getPrefs-default-prefs.json';

/**
 * Gets user preferences.
 *
 * For premium users, return their personal preferences; for free-tier
 * users, return default preferences.
 *
 * @param {User} user
 * @return {Prefs}
 */
function getPrefs(user) {
  const { premium, prefs } = user;

  return premium ? prefs : defaultPrefs;
}

export { getPrefs };
